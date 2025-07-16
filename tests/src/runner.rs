use std::io;

use lutra_bin::br;
use lutra_compiler::ProgramFormat;
use lutra_runner::{Run, binary};

#[tokio::test(flavor = "current_thread")]
async fn main() {
    crate::init_logger();

    let (io_client, io_server) = tokio::io::duplex(1000);

    let runner = lutra_interpreter::InterpreterRunner {
        modules: lutra_interpreter::BUILTIN_MODULES,
    };

    let client = binary::tokio::Client::new(io_client);
    let mut server = binary::tokio::Server::new(io_server, runner);

    // prepare a program
    let source = lutra_compiler::SourceTree::empty();
    let project = lutra_compiler::check(source, Default::default()).unwrap();
    let source = "func (x: int64) -> 3 * x + 2";
    let (program, ty) =
        lutra_compiler::compile(&project, source, None, ProgramFormat::BytecodeLt).unwrap();

    let c = async {
        let res = client
            .execute_raw(&program, &[1, 0, 0, 0, 0, 0, 0, 0])
            .await;
        print_res(res, &ty.output);

        let res = client
            .execute_raw(&program, &[5, 0, 0, 0, 0, 0, 0, 0])
            .await;
        print_res(res, &ty.output);

        client.shutdown().await.unwrap();
    };

    tokio::join!(c, server.run()).1.unwrap();
}

fn print_res(res: Result<Vec<u8>, io::Error>, output_ty: &br::Ty) {
    let res = res.unwrap();
    let res = lutra_bin::Value::decode(&res, output_ty, &[]).unwrap();
    println!("{}", res.print_source(output_ty, &[]).unwrap());
}
