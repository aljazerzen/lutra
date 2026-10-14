#![cfg(feature = "binary-tokio")]

#[cfg(feature = "binary-embedded")]
use lutra_bin::Encode;
use lutra_bin::rr;
use lutra_runner::{
    Run,
    binary::tokio::{Client, Server},
    proto,
};
use std::cell::Cell;
use tokio::io::AsyncWriteExt;

#[tokio::test]
async fn concurrent_requests_and_graceful_cleanup() {
    let (client, server) = tokio::io::duplex(64);
    let runner = Runner(Cell::new(0));
    let client = Client::new(client);
    let mut server = Server::new(server, &runner);
    let calls = async {
        let handle = client.prepare(program()).await.unwrap();
        let (a, b) = tokio::join!(
            client.execute(handle, b"first"),
            client.execute(handle, b"second")
        );
        assert_eq!(a.unwrap(), b"first");
        assert_eq!(b.unwrap(), b"second");
        client.shutdown().await.unwrap();
        assert!(client.pull_schema().await.is_err());
    };
    let (_, result) = tokio::join!(calls, server.run());
    result.unwrap();
    assert_eq!(runner.0.get(), 1);
}

#[tokio::test]
async fn cancelled_exchange_poisons_session_without_deadlocking_shutdown() {
    let (connection, _peer) = tokio::io::duplex(1024);
    let client = Client::new(connection);
    assert!(
        tokio::time::timeout(std::time::Duration::from_millis(10), client.pull_schema())
            .await
            .is_err()
    );
    assert!(client.pull_schema().await.is_err());
    assert!(client.shutdown().await.is_err());
}

#[tokio::test]
async fn malformed_and_truncated_streams_are_errors() {
    for bytes in [b"not an LTDS stream".as_slice(), b"LTDS".as_slice()] {
        let (mut client, server) = tokio::io::duplex(1024);
        let runner = Runner(Cell::new(0));
        let mut server = Server::new(server, &runner);
        let send = async {
            client.write_all(bytes).await.unwrap();
            client.shutdown().await.unwrap();
        };
        let (_, result) = tokio::join!(send, server.run());
        assert!(result.is_err());
    }
}

#[tokio::test]
async fn truncated_connection_releases_prepared_programs() {
    let (connection, server) = tokio::io::duplex(1024);
    let runner = Runner(Cell::new(0));
    let mut server = Server::new(server, &runner);
    let calls = async {
        let client = Client::new(connection);
        client.prepare(program()).await.unwrap();
        drop(client);
    };
    let (_, result) = tokio::join!(calls, server.run());
    assert!(result.is_err());
    assert_eq!(runner.0.get(), 1);
}

#[cfg(feature = "binary-embedded")]
#[tokio::test]
async fn embedded_interop_caches_execute_response() {
    let (connection, server) = tokio::io::duplex(1024);
    let runner = Runner(Cell::new(0));
    let mut server = Server::new(server, &runner);
    let calls = async {
        let mut client = lutra_runner::binary::embedded::Client::new(
            lutra_bin_stream::tokio::TokioIo(connection),
        );
        let program = program().encode();
        assert_eq!(
            client
                .run_once(&program, b"hello".to_vec())
                .await
                .unwrap()
                .0
                .unwrap(),
            b"hello"
        );
        let handle = client.prepare(&program).await.unwrap();
        let a = client.execute(handle, b"a".to_vec()).await.unwrap();
        let b = client.execute(handle, b"b".to_vec()).await.unwrap();
        assert!(client.shutdown().await.is_err());
        client.release(handle).await.unwrap();
        assert_eq!(client.recv_execute(b).await.unwrap().0.unwrap(), b"b");
        assert_eq!(client.recv_execute(a).await.unwrap().0.unwrap(), b"a");
        client.shutdown().await.unwrap();
    };
    let (_, result) = tokio::join!(calls, server.run());
    result.unwrap();
    assert_eq!(runner.0.get(), 2);
}

#[tokio::test]
async fn tiny_duplex_does_not_deadlock_on_headers_or_large_values() {
    let (connection, server) = tokio::io::duplex(1);
    let runner = Runner(Cell::new(0));
    let mut server = Server::new(server, &runner);
    let calls = async {
        let client = Client::new(connection);
        let handle = client.prepare(program()).await.unwrap();
        let value = vec![42; 1024];
        assert_eq!(client.execute(handle, &value).await.unwrap(), value);
        client.shutdown().await.unwrap();
    };
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let (_, result) = tokio::join!(calls, server.run());
        result.unwrap();
    })
    .await
    .unwrap();
    assert!(server.run().await.is_err());
}

#[cfg(feature = "binary-embedded")]
#[tokio::test]
async fn embedded_run_once_receives_large_result_before_release() {
    let (connection, server) = tokio::io::duplex(1);
    let runner = Runner(Cell::new(0));
    let mut server = Server::new(server, &runner);
    let calls = async {
        let mut client = lutra_runner::binary::embedded::Client::new(
            lutra_bin_stream::tokio::TokioIo(connection),
        );
        let value = vec![42; 1024];
        assert_eq!(
            client
                .run_once(&program().encode(), value.clone())
                .await
                .unwrap()
                .0
                .unwrap(),
            value
        );
        client.shutdown().await.unwrap();
    };
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let (_, result) = tokio::join!(calls, server.run());
        result.unwrap();
    })
    .await
    .unwrap();
}

struct Runner(Cell<usize>);
impl Run for Runner {
    async fn prepare(&self, _: rr::Program) -> Result<u32, proto::Error> {
        Ok(42)
    }
    async fn execute(&self, _: u32, input: &[u8]) -> Result<Vec<u8>, proto::Error> {
        Ok(input.to_vec())
    }
    async fn release(&self, _: u32) -> Result<(), proto::Error> {
        self.0.set(self.0.get() + 1);
        Ok(())
    }
    async fn get_externals(&self) -> Result<Vec<String>, proto::Error> {
        Ok(vec![])
    }
}

fn program() -> rr::Program {
    let unit = lutra_bin::ir::Ty {
        kind: lutra_bin::ir::TyKind::Tuple(vec![]),
        layout: None,
        name: None,
        variants_recursive: vec![],
    };
    rr::Program::SqlDuckDb(Box::new(rr::SqlProgram {
        sql: String::new(),
        input_ty: unit.clone(),
        output_ty: unit,
        defs: vec![],
    }))
}
