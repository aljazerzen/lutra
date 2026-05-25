#[test]
fn sync_client_flattens_tuple_input() {
    let mut runner = lutra_interpreter::InterpreterRunner::default();
    let mut root_client = crate::lutra::Client::new_sync(&mut runner);
    let mut client = root_client.runtime();

    let result = client.hello(&1.5, &2).unwrap().unwrap();

    assert_eq!(result, 1);
}
