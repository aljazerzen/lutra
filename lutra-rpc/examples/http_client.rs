use lutra_bin::br;
use lutra_rpc::messages;
use tokio::net::TcpStream;

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let address = std::net::SocketAddr::from(([127, 0, 0, 1], 3000));
    let tcp_io = TcpStream::connect(address).await.unwrap();

    // Create the Hyper client
    let (f, shutdown, rpc_io) = lutra_rpc::http::client(tcp_io).await;

    println!("connected");

    let mut client = lutra_rpc::ClientConnection::new(rpc_io);

    let c = async {
        let source = "let main = func (x: int64) -> 3 * x + 2";
        let program = lutra_compiler::_test_compile(source).unwrap();
        let output_ty = program.get_output_ty().clone();
        let program = lutra_compiler::bytecode_program(program);

        println!("preparing...");
        let program_id = client.prepare(&program).await;

        let res = client
            .run_once(&program, vec![1, 0, 0, 0, 0, 0, 0, 0])
            .await;
        print_res(res, &output_ty);

        let request_id = client
            .execute(program_id, vec![5, 0, 0, 0, 0, 0, 0, 0])
            .await;
        let res = client.recv_response(request_id).await;
        print_res(res, &output_ty);

        client.shutdown().await.unwrap();
        println!("sending shutdown");
        shutdown.send(()).unwrap();
    };

    tokio::join!(f, c);
}

fn print_res(res: messages::Result, output_ty: &br::Ty) {
    let messages::Result::Ok(res) = res else {
        panic!()
    };
    let res = lutra_bin::Value::decode(&res, output_ty, &[]).unwrap();
    println!("{}", res.print_source(output_ty, &[]).unwrap());
}
