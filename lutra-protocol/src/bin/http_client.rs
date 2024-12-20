use lutra_bin::br;
use lutra_protocol::messages;
use tokio::net::TcpStream;

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let address = std::net::SocketAddr::from(([127, 0, 0, 1], 3000));
    let tcp_io = TcpStream::connect(address).await.unwrap();

    // Create the Hyper client
    let (f, shutdown, rpc_io) = lutra_protocol::http::client(tcp_io).await;

    println!("connected");

    let (rpc_rx, rpc_tx) = tokio::io::split(rpc_io);
    let mut client = lutra_protocol::ClientConnection::new(rpc_rx, rpc_tx);

    let c = async {
        let source = "let main = func (x: int64) -> 3 * x + 2";
        let program = lutra_frontend::_test_compile(source);
        let program = lutra_frontend::bytecode_program(program);

        println!("preparing...");
        let program_id = client.prepare(&program).await;

        let res = client
            .run_once(&program, vec![vec![1, 0, 0, 0, 0, 0, 0, 0]])
            .await;
        print_res(res, &program);

        let request_id = client
            .execute(program_id, vec![vec![5, 0, 0, 0, 0, 0, 0, 0]])
            .await;
        let res = client.recv_response(request_id).await;
        print_res(res, &program);

        client.shutdown().await.unwrap();
        println!("sending shutdown");
        shutdown.send(()).unwrap();
    };

    tokio::join!(f, c);
}

fn print_res(res: messages::Result, program: &br::Program) {
    let messages::Result::Ok(res) = res else {
        panic!()
    };
    let res = lutra_bin::Value::decode(&res, &program.output_ty).unwrap();
    println!("{}", res.print_source(&program.output_ty).unwrap());
}
