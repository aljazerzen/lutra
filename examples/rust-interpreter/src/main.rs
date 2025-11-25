// include the file generated in build.rs
mod generated {
    include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

use lutra_interpreter::Run;

#[tokio::main(flavor = "current_thread")]

async fn main() {
    let runner = lutra_interpreter::InterpreterRunner::default();

    let res = runner
        .run(&generated::get_movies(), &())
        .await
        .unwrap()
        .unwrap();

    println!("Result: {res:#?}");
}
