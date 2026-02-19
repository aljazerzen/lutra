_default:
    just --list

# Show crate dependencies within the workspace
show-deps-workspace:
    cargo depgraph --workspace-only --hide tests,lutra-cli,lutra-tui > ./target/deps-workspace.dot
    dot ./target/deps-workspace.dot -Tsvg -o ./target/graph.svg -Gnodesep=0.5
    xdg-open ./target/graph.svg 2> /dev/null

# Start PostgreSQL
pg-up:
    podman run -e POSTGRES_PASSWORD=pass -p 5416:5432 docker.io/library/postgres:latest

# Run tests, insta review, fmt, clippy
dev FILTER_SET='all()' *NEXTEST_ARGS='':
    # RUST_BACKTRACE=1
    # RUST_LOG=lutra_compiler::sql=debug
    RUST_LOG=debug \
    INSTA_FORCE_PASS=1 \
    cargo nextest run -E '{{FILTER_SET}}' {{NEXTEST_ARGS}}
    cargo insta review

    cargo fmt
    cargo check --all-targets --profile=test
    cargo clippy --all-targets
    cargo check -p lutra-bin --no-default-features

# Run ignored tests
test-ignored:
    INSTA_FORCE_PASS=1 \
    cargo nextest --profile=overview run --run-ignored=only

# Re-generate committed generated files
[working-directory: 'lutra-bin/src/project']
generate:
    cargo run -p lutra-cli -- codegen --lutra-bin-path="crate" --project=. ./generated.rs
    cargo fmt -p lutra-bin

[working-directory: 'lutra-bin/src/project']
generate-precompiled:
    # For when current code does not compile, but we still have an old cli binary
    ../../../target/debug/lutra codegen --lutra-bin-path="crate" --project=. ./generated.rs
    cargo fmt -p lutra-bin

# Test (this should pass for every commit)
test:
    cargo nextest run --profile=overview
    just py test
    just website/test

# Publish crates
publish:
    cargo publish -p lutra-bin
    cargo publish -p sql-ast
    cargo publish -p lutra-compiler
    cargo publish -p lutra-codegen
    cargo publish -p lutra-runner
    cargo publish -p lutra-runner-postgres
    cargo publish -p lutra-arrow
    cargo publish -p lutra-runner-duckdb
    cargo publish -p lutra-interpreter
    cargo publish -p lutra-tui
    cargo publish -p lutra-cli

# Run a command for each Python package
py *ARGS='':
    just lutra-bin/python/{{ARGS}}
    just lutra-runner-postgres/python/{{ARGS}}

