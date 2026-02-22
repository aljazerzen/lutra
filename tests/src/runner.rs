use std::io;

use lutra_bin::br;
use lutra_compiler::ProgramFormat;
use lutra_runner::{AsyncRunner, Run, binary};

#[tokio::test(flavor = "current_thread")]
async fn main() {
    crate::init_logger();

    let (io_client, io_server) = tokio::io::duplex(1000);

    let runner = lutra_interpreter::InterpreterRunner::default();
    let runner = AsyncRunner::new(runner);

    let client = binary::tokio::Client::new(io_client);
    let mut server = binary::tokio::Server::new(io_server, runner);

    // prepare a program
    let source = lutra_compiler::SourceTree::empty();
    let project = lutra_compiler::check(source, Default::default()).unwrap();
    let source = "func (x: int64) -> 3 * x + 2";
    let (program, ty) =
        lutra_compiler::compile(&project, source, None, ProgramFormat::BytecodeLt).unwrap();

    let c = async {
        let program = client.prepare(program).await.unwrap();

        let res = client.execute(&program, &[1, 0, 0, 0, 0, 0, 0, 0]).await;
        assert_eq!("5", res_to_string(res, &ty.output));

        let res = client.execute(&program, &[5, 0, 0, 0, 0, 0, 0, 0]).await;
        assert_eq!("17", res_to_string(res, &ty.output));

        client.shutdown().await.unwrap();
    };

    tokio::join!(c, server.run()).1.unwrap();
}

fn res_to_string(res: Result<Vec<u8>, io::Error>, output_ty: &br::Ty) -> String {
    let res = res.unwrap();
    lutra_bin::print_source(&res, output_ty, &[]).unwrap()
}
