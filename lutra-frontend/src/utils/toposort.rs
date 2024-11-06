use std::collections::HashMap;

use itertools::Itertools;

type Dag = Vec<Vec<usize>>;

struct DepthFirstSearch {
    nodes: Vec<NodeStatus>,
    finish_order: Vec<usize>,
}

#[derive(Clone, Copy)]
struct NodeStatus {
    visiting: bool,
    finished: bool,
}

/// Performs a topological sort + computes strongly connected components
/// of the DAG specified by the `dependencies`.
pub fn toposort<'a, Key: Eq + std::hash::Hash + Clone>(
    dependencies: &'a [(Key, Vec<Key>)],
) -> Vec<Vec<&'a Key>> {
    // create mapping from Key to usize
    let index: HashMap<&Key, usize> = dependencies
        .iter()
        .enumerate()
        .map(|(index, (key, _))| (key, index))
        .collect();

    // map DAG from Key to usize
    let dag: Dag = dependencies
        .iter()
        .map(|(_, deps)| deps.iter().flat_map(|d| index.get(d).cloned()).collect())
        .collect();

    // first dfs
    let mut dfs = DepthFirstSearch::new(dag.len());
    dfs.visit_all(&dag);

    // transpose and sort edges by finished_at
    let dag_t = transpose_dag(&dag);

    // second dfs
    let mut dfs_2 = DepthFirstSearch::new(dag.len());
    let trees = dfs_2.visit_in_order(&dag_t, dfs.finish_order.iter().cloned().rev());

    // unmap
    trees
        .iter()
        .rev()
        .map(|tree| tree.iter().map(|i| &dependencies[*i].0).collect_vec())
        .collect()
}

impl DepthFirstSearch {
    fn new(number_of_nodes: usize) -> Self {
        const EMPTY: NodeStatus = NodeStatus {
            visiting: false,
            finished: false,
        };

        DepthFirstSearch {
            nodes: vec![EMPTY; number_of_nodes],
            finish_order: Vec::with_capacity(number_of_nodes),
        }
    }

    fn visit(&mut self, dag: &Dag, n: usize) {
        let node = &mut self.nodes[n];
        if node.finished {
            return;
        }
        if node.visiting {
            return;
        }
        node.visiting = true;

        for m in &dag[n] {
            self.visit(dag, *m);
        }

        let node = &mut self.nodes[n];
        node.visiting = false;
        node.finished = true;
        self.finish_order.push(n);
    }

    /// Start visits from each node, until all nodes have been visited
    fn visit_all(&mut self, dag: &Dag) {
        for start_at in 0..dag.len() {
            self.visit(&dag, start_at);
            if self.finish_order.len() == dag.len() {
                break;
            }
        }
    }

    /// Start visits from nodes provided by the iterator.
    /// Returns trees produced by each visit, each tree in finish_order.
    fn visit_in_order(&mut self, dag: &Dag, order: impl Iterator<Item = usize>) -> Vec<Vec<usize>> {
        let mut last = 0_usize;
        let mut trees = Vec::new();

        for start_at in order {
            self.visit(&dag, start_at);

            if self.finish_order.len() > last {
                trees.push(self.finish_order[last..].to_vec());
                last = self.finish_order.len();
            }

            if self.finish_order.len() == dag.len() {
                break;
            }
        }
        trees
    }
}

fn transpose_dag(dag: &Dag) -> Dag {
    // count incoming edges for each edge
    let mut incoming_edges = vec![0; dag.len()];
    for edges in dag {
        for v in edges {
            incoming_edges[*v] += 1;
        }
    }

    // init edges for each node
    let mut dag_t: Dag = Vec::with_capacity(dag.len());
    for ie in incoming_edges {
        dag_t.push(Vec::with_capacity(ie));
    }

    // transpose
    for (u, edges) in dag.iter().enumerate() {
        for v in edges {
            dag_t[*v].push(u);
        }
    }

    dag_t
}

#[cfg(test)]
mod tests {
    use super::toposort;

    #[test]
    fn normal_sort() {
        let dependencies = vec![
            ("a", vec!["b"]),
            ("b", vec!["c"]),
            ("c", vec![]),
            ("d", vec![]),
        ];
        let order = toposort(&dependencies);

        assert_eq!(order, vec![vec![&"c"], vec![&"b"], vec![&"a"], vec![&"d"]]);
    }

    #[test]
    fn normal_sort_2() {
        let dependencies = vec![
            ("a", vec![]),
            ("b", vec![]),
            ("c", vec!["b"]),
            ("d", vec!["c"]),
        ];
        let order = toposort(&dependencies);

        assert_eq!(order, vec![vec![&"a"], vec![&"b"], vec![&"c"], vec![&"d"]]);
    }

    #[test]
    fn dag_with_cycle() {
        let dependencies = vec![
            ("a", vec!["b"]),
            ("b", vec!["c", "d"]),
            ("c", vec![]),
            ("d", vec!["a"]),
        ];
        let order = toposort(&dependencies);

        assert_eq!(order, vec![vec![&"c"], vec![&"b", &"d", &"a"]]);
    }

    #[test]
    fn parallel_when_ambiguous() {
        let dependencies = vec![
            ("a", vec!["b"]),
            ("b", vec![]),
            ("c", vec!["b"]),
            ("d", vec!["b"]),
        ];

        let order = toposort(&dependencies);

        assert_eq!(order, vec![vec![&"b"], vec![&"a"], vec![&"c"], vec![&"d"]]);
    }
}
