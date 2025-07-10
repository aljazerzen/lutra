use std::convert::Infallible;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use http_body_util::{BodyExt, Full};
use hyper::body::{Bytes, Incoming};
use hyper::client::conn::http1 as http1_c;
use hyper::server::conn::http1 as http1_s;
use hyper::service::Service;
use hyper::{Request, Response};
use hyper_util::rt::TokioIo;
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt, DuplexStream};
use tokio::sync::{Mutex, oneshot};

pub fn server(
    tcp_io: impl AsyncRead + AsyncWrite + Unpin,
) -> (impl Future<Output = ()>, DuplexStream) {
    let tcp_io = TokioIo::new(tcp_io);

    let (server, inner_stream) = Server::new();

    let serve = async {
        http1_s::Builder::new()
            .serve_connection(tcp_io, server)
            .await
            .unwrap()
    };

    (serve, inner_stream)
}

struct Server {
    into_http: Arc<Mutex<DuplexStream>>,
}

impl Server {
    fn new() -> (Self, DuplexStream) {
        let (into_http, into_rpc) = tokio::io::duplex(1024);
        let into_http = Arc::new(Mutex::new(into_http));
        (Self { into_http }, into_rpc)
    }
}

impl Service<Request<Incoming>> for Server {
    type Response = Response<Full<Bytes>>;

    type Error = Infallible;

    type Future = Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>> + Send>>;

    fn call(&self, mut req: Request<Incoming>) -> Self::Future {
        let into_http = self.into_http.clone();

        log::debug!("call");

        let r = async move {
            let incoming = req.body_mut();
            while let Some(frame) = incoming.frame().await {
                let frame = frame.unwrap();
                log::debug!("frame: {frame:?}");

                if let Ok(data) = frame.into_data() {
                    into_http.lock().await.write_all(&data).await.unwrap();
                }
            }

            log::debug!("waiting for response");
            let mut buf = [0; 1024];
            let bytes_read = into_http.lock().await.read(&mut buf).await.unwrap();
            log::debug!("sending response of {bytes_read} bytes");
            Ok(Response::builder()
                .header("Access-Control-Allow-Origin", "*")
                .body(Full::new(Bytes::from_owner(buf[0..bytes_read].to_vec())))
                .unwrap())
        };
        Box::pin(r)
    }
}

pub async fn client(
    tcp_io: impl AsyncRead + AsyncWrite + Unpin + Send + 'static,
) -> (impl Future<Output = ()>, oneshot::Sender<()>, DuplexStream) {
    let tcp_io = TokioIo::new(tcp_io);

    let (shutdown_tx, shutdown_rx) = oneshot::channel();

    let (sender, conn) = http1_c::Builder::new()
        .handshake::<_, Full<Bytes>>(tcp_io)
        .await
        .unwrap();

    let (client, into_rpc) = Client::new(sender, shutdown_rx);

    let f = async move {
        let (_, _) = tokio::join!(
            async move {
                let mut client = client;
                client.run().await;
                client.close().await;
            },
            conn
        );
    };

    (f, shutdown_tx, into_rpc)
}

struct Client {
    into_http: Arc<Mutex<DuplexStream>>,
    sender: http1_c::SendRequest<Full<Bytes>>,
    shutdown_rx: oneshot::Receiver<()>,
}

impl Client {
    fn new(
        sender: http1_c::SendRequest<Full<Bytes>>,
        shutdown_rx: oneshot::Receiver<()>,
    ) -> (Self, DuplexStream) {
        let (into_http, into_rpc) = tokio::io::duplex(1024);

        let into_http = Arc::new(Mutex::new(into_http));

        (
            Self {
                into_http,
                shutdown_rx,
                sender,
            },
            into_rpc,
        )
    }

    async fn run(&mut self) {
        let into_http = self.into_http.clone();

        loop {
            if self.shutdown_rx.try_recv().is_ok() {
                break;
            }

            log::debug!("waiting for rpc get packets to be sent");
            let mut buf = [0; 1024];
            let Ok(bytes_read) = into_http.lock().await.read(&mut buf).await else {
                break;
            };
            if bytes_read == 0 {
                continue;
            }
            let body = Full::new(Bytes::from_owner(buf[0..bytes_read].to_vec()));

            log::debug!("sending {body:?}");

            let req = Request::builder()
                .method("POST")
                .header("Content-Type", "application/lutra-rpc")
                .body(body)
                .unwrap();

            let mut response = self.sender.send_request(req).await.unwrap();

            let incoming = response.body_mut();
            while let Some(frame) = incoming.frame().await {
                let frame = frame.unwrap();
                log::debug!("incoming: {frame:?}");

                if let Ok(data) = frame.into_data() {
                    into_http.lock().await.write_all(&data).await.unwrap();
                }
            }
            log::debug!("request sent");
        }
    }

    async fn close(&mut self) {
        self.into_http.lock().await.shutdown().await.unwrap();
    }
}
