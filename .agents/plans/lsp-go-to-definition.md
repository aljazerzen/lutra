# LSP: Go to Definition (#108)

Implement `textDocument/definition` in the Lutra language server.

## Overview

Given a cursor position, the LSP server must find the definition site of the
identifier under the cursor and return its location. The key challenge is doing
this without cloning the resolved PR AST on every request.

The solution is a **`TargetMap`**: a compact index built once after each
compilation that maps source spans to their resolved `pr::Ref` targets.

---

## Data structures

### `TargetMap` — new, in `lutra-compiler/src/project/analysis.rs`

```rust
pub struct TargetMap {
    // Keyed by source_id. Each vec is sorted by TargetEntry::start.
    by_source: HashMap<u16, Vec<TargetEntry>>,
}

struct TargetEntry {
    start: u32,
    len: u16,
    target: Ref,
}
```

**Why `HashMap` and not `Vec`:** source IDs are currently sequential indices
into an `IndexMap`, so a `Vec` would work today. But that's an implementation
detail that could change, and `u16::MAX` is already used as a synthetic ID for
`SourceOverlay`. Using `HashMap` avoids this footgun.

**Query** — `TargetMap::find_at(source_id: u16, offset: u32) -> Option<&Ref>`:
1. Look up `by_source[source_id]`.
2. Use `partition_point` to find the first entry with `start > offset`,
   giving the slice of candidates with `start <= offset`.
3. Among those, filter for `start + len > offset` (span actually contains the
   cursor).
4. Return the one with the smallest `len` (innermost / most specific span).

### `Project` — add `target_map` field

```rust
pub struct Project {
    pub source: SourceTree,
    pub root_module: pr::ModuleDef,
    pub ordering: Vec<Vec<pr::Path>>,
    pub dependencies: Vec<Dependency>,
    pub target_map: TargetMap,   // new
}
```

---

## Building the TargetMap

The `NameResolver` (`lutra-compiler/src/resolver/names/expr.rs`) is the single
place where `target` is set on both `pr::Expr` and `pr::Ty` nodes. Add a
`Vec<TargetEntry>` accumulator to `NameResolver` and push an entry every time
a target is set (when `span` is available).

After all sources are resolved, sort each per-source vec by `start` and wrap
into `TargetMap`. The per-source structure is a natural fit: each `NameResolver`
run operates within one source file.

Overlay sources (`SourceOverlay::overlay_id() == u16::MAX`) are ephemeral and
must not be stored in the `TargetMap`.

---

## Finding the definition span

### `find_definition` — public function in `analysis.rs`

```rust
pub fn find_definition(project: &Project, source_id: u16, offset: u32) -> Option<Span>
```

Steps:
1. Call `project.target_map.find_at(source_id, offset)`.
2. Match on the returned `Ref`:
   - `Ref::Global(path)` → call `find_global_def_span(&project.root_module, path)`.
   - `Ref::Local { .. }` → return `None` (out of scope for now).
3. Return the span.

### `find_global_def_span` — private helper in `analysis.rs`

```rust
fn find_global_def_span(module: &ModuleDef, path: &Path) -> Option<Span>
```

Walks `path.as_steps()` through `module.defs`, returning
`def.span_name.or(def.span)` of the final `Def`.

---

## Compiler exports

In `lutra-compiler/src/project/mod.rs`:
```rust
mod analysis;
pub use analysis::find_definition;
```

In `lutra-compiler/src/lib.rs`:
```rust
pub use project::find_definition;
```

(`project.rs` is converted to `project/mod.rs` + `project/analysis.rs`.)

---

## LSP wiring — `lutra-cli/src/language_server.rs`

**`initialize`** — add to `ServerCapabilities`:
```rust
definition_provider: Some(OneOf::Left(true)),
```

**New trait method** on `Backend`:
```rust
async fn goto_definition(
    &self,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>>
```

Implementation:
1. Find the project for the URI (same pattern as `formatting`).
2. Get `source_id` from `project_lock.source_tree`.
3. Get the source text from the document lock.
4. Convert cursor `Position` → byte offset via `LineNumbers::byte_index_of_line_col`.
5. Call `lutra_compiler::find_definition(checked_project, source_id, offset)`.
6. Convert the returned `Span` → `Uri` + `Range` via existing `to_proto::source_id`
   and `to_proto::location` helpers.
7. Return `Ok(Some(GotoDefinitionResponse::Scalar(location)))`.

Return `Ok(None)` when the project has not been checked yet, the cursor is not
on an identifier, or the definition has no source span (e.g. a built-in).

---

## Possible optimization: `Rc<Ref>` sharing

Currently, `Ref::Global(path)` contains a `Vec<String>` (heap-allocated). When
building the `TargetMap`, each entry clones the `Ref` from the AST node —
allocating a new `Vec<String>` per entry.

If `pr::Expr::target`, `pr::Ty::target`, and `TargetEntry::target` were changed
from `Option<Ref>` to `Option<Rc<Ref>>`, the `TargetMap` build step would only
clone the `Rc` (a pointer bump) instead of deep-cloning the path. Each `Ref`
would be allocated once and shared between the AST node and the `TargetMap`.

This reduces both memory usage and allocation cost during compilation, at the
expense of an extra indirection on access. Worth considering once the feature is
working correctly.
