use lutra_bin::br;
use lutra_rpc::messages;

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let (io_client, io_server) = tokio::io::duplex(1000);

    let mut client = lutra_rpc::ClientConnection::new(io_client);
    let mut server =
        lutra_rpc::ServerConnection::new(io_server, lutra_interpreter::BUILTIN_MODULES.to_vec());

    let source = "let main = func (x: int64) -> 3 * x + 2";
    let program = lutra_compiler::_test_compile(source).unwrap();
    let output_ty = program.get_output_ty().clone();
    let program = lutra_compiler::bytecode_program(program);

    let c = async {
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
    };

    tokio::join!(c, server.run());
}

fn print_res(res: messages::Result, output_ty: &br::Ty) {
    let messages::Result::Ok(res) = res else {
        panic!()
    };
    let res = lutra_bin::Value::decode(&res, output_ty, &[]).unwrap();
    println!("{}", res.print_source(output_ty, &[]).unwrap());
}
