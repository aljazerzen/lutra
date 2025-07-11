_default:
    just --list

show-deps-workspace:
    cargo depgraph --workspace-only > ./target/deps-workspace.dot
    dot ./target/deps-workspace.dot -Tsvg -o ./target/graph.svg
    xdg-open ./target/graph.svg 2> /dev/null

test-fast FILTER_SET='all()' *NEXTEST_ARGS='':
    # RUST_BACKTRACE=1
    RUST_LOG=debug INSTA_FORCE_PASS=1 \
    cargo nextest run --no-fail-fast -E '{{FILTER_SET}}' {{NEXTEST_ARGS}}
    cargo insta review

    cargo fmt
    cargo check --all-targets --profile=test
    cargo clippy --all-targets
    cargo check -p lutra-bin --no-default-features

[working-directory: 'lutra-bin/src/project']
generate:
    cargo run -p lutra-cli -- codegen --lutra-bin-path="crate" . ./generated.rs
    cargo fmt -p lutra-bin

[working-directory: 'lutra-bin/src/project']
generate-precompiled:
    # For when current code does not compile, but we still have an old cli binary
    ../../../target/debug/lutra codegen --lutra-bin-path="crate" . ./generated.rs
    cargo fmt -p lutra-bin
