use std::collections::HashMap;

use crate::Span;
use crate::pr::Path;

use super::Project;

/// Information about a symbol at a given source position.
///
/// Returned by [`Project::find_by_span`] and used by LSP features such as
/// go-to-definition and hover.
pub struct SymbolInfo {
    /// Span of the identifier token at the queried position.
    pub source_span: Span,
    /// Path of the referenced global definition, or `None`
    /// for local bindings.
    pub target_path: Option<Path>,
    /// Span of the definition site (go-to-definition target).
    pub target_span: Option<Span>,
}

impl Project {
    /// Look up the symbol at `(source_id, offset)` — a byte offset within a
    /// source file — and return information about what it refers to.
    ///
    /// Returns `None` when the offset does not correspond to a known identifier.
    pub fn find_by_span(&self, source_id: u16, offset: u32) -> Option<SymbolInfo> {
        let (target, source_span) = self.target_map.find_at(source_id, offset)?;
        Some(match target {
            TargetSpan::Global(path) => {
                let target_span = self.root_module.get(path).and_then(|d| d.span_name);
                SymbolInfo {
                    source_span,
                    target_path: Some(path.clone()),
                    target_span,
                }
            }
            TargetSpan::Span(target_span) => SymbolInfo {
                source_span,
                target_path: None,
                target_span: Some(*target_span),
            },
        })
    }
}

/// An index of all resolved identifier references in a compiled project,
/// keyed by source location.  Built once after name resolution and used for
/// queries like go-to-definition.
#[derive(Debug, Default)]
pub struct TargetMap {
    /// One sorted vec per source file (keyed by source_id).
    /// Each vec is sorted by `TargetEntry::start` to allow binary search.
    by_source: HashMap<u16, Vec<TargetEntry>>,
}

#[derive(Debug)]
struct TargetEntry {
    start: u32,
    len: u16,
    target: TargetSpan,
}

#[derive(Debug)]
pub enum TargetSpan {
    Global(Path),
    Span(Span),
}

impl TargetMap {
    /// Build a `TargetMap` from a flat list of `(span, target)` pairs collected
    /// during name resolution.  Overlay source IDs (`u16::MAX`) are excluded.
    pub fn build(entries: Vec<(Span, TargetSpan)>) -> Self {
        let mut by_source: HashMap<u16, Vec<TargetEntry>> = HashMap::new();

        for (span, target) in entries {
            let entry = by_source.entry(span.source_id).or_default();
            entry.push(TargetEntry {
                start: span.start,
                len: span.len,
                target,
            });
        }

        for vec in by_source.values_mut() {
            vec.sort_by_key(|e| e.start);
        }

        TargetMap { by_source }
    }

    /// Find the resolved target for the identifier at `(source_id, offset)`.
    ///
    /// Returns both the [`TargetSpan`] and the source [`Span`] of the matched
    /// token (i.e. the span of the identifier itself, useful as the LSP hover
    /// range).
    ///
    /// When multiple spans contain the offset (e.g. nested expressions share a
    /// start position), the innermost one (smallest `len`) is returned.
    pub fn find_at(&self, source_id: u16, offset: u32) -> Option<(&TargetSpan, Span)> {
        let entries = self.by_source.get(&source_id)?;

        // All entries with start <= offset are candidates; find the boundary.
        let end = entries.partition_point(|e| e.start <= offset);

        entries[..end]
            .iter()
            .filter(|e| e.start + e.len as u32 > offset)
            .min_by_key(|e| e.len)
            .map(|e| {
                let span = Span {
                    source_id,
                    start: e.start,
                    len: e.len,
                };
                (&e.target, span)
            })
    }
}
