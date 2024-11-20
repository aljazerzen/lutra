
show-deps-workspace:
    cargo depgraph --workspace-only > ./target/deps-workspace.dot
    dot ./target/deps-workspace.dot -Tsvg -o ./target/graph.svg
    xdg-open ./target/graph.svg 2> /dev/null
