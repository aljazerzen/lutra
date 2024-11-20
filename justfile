
show-deps-workspace:
    cargo depgraph --workspace-only > ./target/deps-workspace.dot
    dot ./target/deps-workspace.dot -Tsvg -o ./target/graph.svg
    xdg-open ./target/graph.svg 2> /dev/null

test-fast tests='':
    cargo fmt
    cargo check --all-targets --profile=test
    INSTA_FORCE_PASS=1 cargo nextest run -- {{tests}}
    cargo insta review
    cargo clippy --all-targets
