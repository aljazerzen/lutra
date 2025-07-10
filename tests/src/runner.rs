use std::io;

use lutra_bin::{Encode, br};
use lutra_runner::{Run, binary};

#[tokio::test(flavor = "current_thread")]
async fn main() {
    let (io_client, io_server) = tokio::io::duplex(1000);

    let runner = lutra_interpreter::InterpreterRunner {
        modules: lutra_interpreter::BUILTIN_MODULES,
    };

    let client = binary::tokio::Client::new(io_client);
    let mut server = binary::tokio::Server::new(io_server, runner);

    let source = "let main = func (x: int64) -> 3 * x + 2";
    let program = lutra_compiler::_test_compile(source).unwrap();
    let output_ty = program.get_output_ty().clone();
    let program = lutra_compiler::bytecode_program(program);
    let program = lutra_bin::Program {
        format: "bytecode-lt".into(),
        inner: program.encode(),
    };

    let c = async {
        let res = client
            .execute_raw(&program, &[1, 0, 0, 0, 0, 0, 0, 0])
            .await;
        print_res(res, &output_ty);

        let res = client
            .execute_raw(&program, &[5, 0, 0, 0, 0, 0, 0, 0])
            .await;
        print_res(res, &output_ty);

        client.shutdown().await.unwrap();
    };

    tokio::join!(c, server.run()).1.unwrap();
}

fn print_res(res: Result<Vec<u8>, io::Error>, output_ty: &br::Ty) {
    let res = res.unwrap();
    let res = lutra_bin::Value::decode(&res, output_ty, &[]).unwrap();
    println!("{}", res.print_source(output_ty, &[]).unwrap());
}
