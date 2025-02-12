use lutra_bin::br;
use lutra_protocol::messages;

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let (io_client, io_server) = tokio::io::duplex(1000);

    let mut client = lutra_protocol::ClientConnection::new(io_client);
    let mut server =
        lutra_protocol::ServerConnection::new(io_server, lutra_runtime::BUILTIN_MODULES.to_vec());

    let source = "let main = func (x: int64) -> 3 * x + 2";
    let program = lutra_frontend::_test_compile(source);
    let program = lutra_frontend::bytecode_program(program);

    let c = async {
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
    };

    tokio::join!(c, server.run());
}

fn print_res(res: messages::Result, program: &br::Program) {
    let messages::Result::Ok(res) = res else {
        panic!()
    };
    let res = lutra_bin::Value::decode(&res, &program.output_ty).unwrap();
    println!("{}", res.print_source(&program.output_ty).unwrap());
}
