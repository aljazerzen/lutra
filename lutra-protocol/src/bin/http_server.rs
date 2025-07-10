#[tokio::main(flavor = "current_thread")]
async fn main() {
    let addr = std::net::SocketAddr::from(([127, 0, 0, 1], 3000));
    let listener = tokio::net::TcpListener::bind(addr).await.unwrap();

    loop {
        let (stream, _) = listener.accept().await.unwrap();

        let (http_serve, rpc_io) = lutra_protocol::http::server(stream);

        let mut server =
            lutra_protocol::ServerConnection::new(rpc_io, lutra_interpreter::BUILTIN_MODULES.to_vec());

        tokio::join!(server.run(), http_serve);
    }
}
