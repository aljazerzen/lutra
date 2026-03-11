use std::collections::HashMap;

use crate::Span;
use crate::pr::Path;

use super::Project;

impl Project {
    /// Find the span of definition of the expression that is at the given
    /// byte offset in a given source file.
    pub fn find_target(&self, source_id: u16, offset: u32) -> Option<Span> {
        let r = self.target_map.find_at(source_id, offset)?;

        match r {
            TargetSpan::Global(path) => self.root_module.get(path).and_then(|d| d.span_name),
            TargetSpan::Span(span) => Some(*span),
        }
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
    /// When multiple spans contain the offset (e.g. nested expressions share a
    /// start position), the innermost one (smallest `len`) is returned.
    pub fn find_at(&self, source_id: u16, offset: u32) -> Option<&TargetSpan> {
        let entries = self.by_source.get(&source_id)?;

        // All entries with start <= offset are candidates; find the boundary.
        let end = entries.partition_point(|e| e.start <= offset);

        entries[..end]
            .iter()
            .filter(|e| e.start + e.len as u32 > offset)
            .min_by_key(|e| e.len)
            .map(|e| &e.target)
    }
}
