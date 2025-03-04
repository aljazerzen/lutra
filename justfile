_default:
    just --list

show-deps-workspace:
    cargo depgraph --workspace-only > ./target/deps-workspace.dot
    dot ./target/deps-workspace.dot -Tsvg -o ./target/graph.svg
    xdg-open ./target/graph.svg 2> /dev/null

test-fast tests='':
    cargo fmt
    cargo check --all-targets --profile=test

    RUST_LOG=debug RUST_BACKTRACE=0 INSTA_FORCE_PASS=1 \
    cargo nextest run --no-fail-fast -- {{tests}}
    cargo insta review

    cargo clippy --all-targets
    cargo check -p lutra-bin --no-default-features

generate:
    cargo run -p lutra-codegen -- lutra-bin/src/project lutra-bin/src/project/generated.rs
    cargo fmt -p lutra-bin
    ./target/debug/lutra-codegen lutra-bin/src/project lutra-bin/src/project/generated.rs
    cargo fmt -p lutra-bin
