mod lutra {
    include!(concat!(env!("OUT_DIR"), "/lutra.rs"));
}

use lutra_bin::sr::SyncRun;

fn main() {
    let mut client = lutra_db_driver::RunnerSync(
        postgres::Client::connect("postgres://postgres:pass@localhost:5416", postgres::NoTls)
            .unwrap(),
    );

    let res = client.run(&lutra::get_movies(), &()).unwrap();

    println!("{res:#?}");
}
