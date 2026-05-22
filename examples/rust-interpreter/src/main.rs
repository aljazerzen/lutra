// include the file generated in build.rs
mod generated {
    #![allow(dead_code)]
    include!(concat!(env!("OUT_DIR"), "/lutra.rs"));
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let mut runner = lutra_interpreter::InterpreterRunner::default();
    let mut client = generated::Client::new_sync(&mut runner);

    let res = client.get_movies().unwrap().unwrap();

    println!("Result: {res:#?}");
}
