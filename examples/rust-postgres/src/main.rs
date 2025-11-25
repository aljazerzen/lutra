// include the file generated in build.rs
mod generated {
    include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

use lutra_runner_postgres::Run;

#[tokio::main(flavor = "current_thread")]
async fn main() {
    // init PostgreSQL runner
    let client = lutra_runner_postgres::RunnerAsync::connect_no_tls(
        "postgres://postgres:pass@localhost:5416",
    )
    .await
    .unwrap();

    // insert a few movies
    let movies = vec![
        generated::Movie {
            id: 54,
            title: "Hello".into(),
            is_released: true,
        },
        generated::Movie {
            id: 3,
            title: "world".into(),
            is_released: false,
        },
    ];
    client
        .run(&generated::insert_movies(), &movies)
        .await
        .unwrap()
        .unwrap();

    // fetch movies
    let res = client
        .run(&generated::get_movies(), &())
        .await
        .unwrap()
        .unwrap();
    println!("{res:#?}");
}
