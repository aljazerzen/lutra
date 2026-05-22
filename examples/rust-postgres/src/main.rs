// include the file generated in build.rs
mod generated {
    #![allow(dead_code)]
    include!(concat!(env!("OUT_DIR"), "/lutra.rs"));
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    // init PostgreSQL runner
    let runner = lutra_runner_postgres::RunnerAsync::connect_no_tls(
        "postgres://postgres:pass@localhost:5416",
    )
    .await
    .unwrap();
    let client = generated::Client::new(&runner);

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
    client.insert_movies(&movies).await.unwrap().unwrap();

    // fetch movies
    let res = client.get_movies().await.unwrap().unwrap();
    println!("{res:#?}");
}
