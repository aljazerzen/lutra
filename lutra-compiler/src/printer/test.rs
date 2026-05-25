#![cfg(test)]

use insta::assert_snapshot;

#[track_caller]
fn _format(source: &str) -> String {
    let (parsed, dia, trivia) = crate::parser::parse_source(source, 0);

    if !dia.is_empty() {
        panic!("parse err: {dia:?}");
    }

    let Some(parsed) = parsed else {
        panic!("parse err: {dia:?}");
    };

    let edits = super::print_source(&parsed, Some(&trivia));
    crate::codespan::apply_text_edits(source, &edits)
}

#[track_caller]
fn _print_edits(source: &str) -> (crate::pr::Source, Vec<crate::codespan::TextEdit<'_>>) {
    let (parsed, dia, trivia) = crate::parser::parse_source(source, 0);

    if !dia.is_empty() {
        panic!("parse err: {dia:?}");
    }

    let Some(parsed) = parsed else {
        panic!("parse err: {dia:?}");
    };

    let edits = super::print_source(&parsed, Some(&trivia));
    (parsed, edits)
}

#[test]
fn format_00() {
    assert_snapshot!(_format(r#"
    const   x
     =      [
        hello::world,

                       ]
    "#),
    @"const x = [hello::world]")
}

#[test]
fn format_01() {
    assert_snapshot!(_format(r#"
    const x = [hello::world, hello::world, hello::world, hello::world, hello::world]
    "#),
    @"const x = [hello::world, hello::world, hello::world, hello::world, hello::world]"
    )
}

#[test]
fn format_02() {
    assert_snapshot!(_format(r#"
    const long_name = [hello::world, hello::world, hello::world, hello::world, hello::world]
    "#),
    @"
    const long_name = [
      hello::world, hello::world, hello::world, hello::world, hello::world,
    ]
    "
    )
}

#[test]
fn format_03() {
    assert_snapshot!(_format(r#"
    const x = [
      hello::world, hello::world, hello::world, hello::world, hello::world, hello::world
    ]
    "#), @r"
    const x = [
      hello::world,
      hello::world,
      hello::world,
      hello::world,
      hello::world,
      hello::world,
    ]
    "
    )
}

#[test]
fn format_04() {
    assert_snapshot!(_format(r#"
    const x = [[hello::world, hello::world, hello::world, hello::world, hello::world]]
    "#), @"
    const x = [
      [hello::world, hello::world, hello::world, hello::world, hello::world],
    ]
    "
    )
}

#[test]
fn format_05() {
    assert_snapshot!(_format(r#"
    const x = [[hello::world, hello::world, hello::world], [hello::world, hello::world]]
    "#), @"
    const x = [
      [hello::world, hello::world, hello::world], [hello::world, hello::world],
    ]
    "
    )
}

#[test]
fn format_06() {
    assert_snapshot!(_format(r#"
    const x = [hello::world, [hello::world, hello::world, hello::world], hello::world]
    "#), @"
    const x = [
      hello::world, [hello::world, hello::world, hello::world], hello::world,
    ]
    "
    )
}

#[test]
fn format_07() {
    assert_snapshot!(_format(r#"
    const long_long_name = ([hello::world, hello::world, hello::world, hello::world])
    "#), @r"
    const long_long_name = (
      [hello::world, hello::world, hello::world, hello::world]
    )
    "
    )
}

#[test]
fn format_08() {
    assert_snapshot!(_format(r#"
    const x = [
      hello::world,
      [hello::world, hello::world, hello::world, hello::world, hello::world, longer],
    ]
    "#), @r"
    const x = [
      hello::world,
      [
        hello::world,
        hello::world,
        hello::world,
        hello::world,
        hello::world,
        longer,
      ],
    ]
    "
    )
}

#[test]
fn format_09() {
    assert_snapshot!(_format(r#"
const stats = {
  total_tasks = std::count(task_ops::tasks),
  avg_priority = (
    task_ops::tasks
    | std::map(t -> std::to_int64(t.priority))
    | std::mean()
  ),
}
    "#), @"
    const stats = {
      total_tasks = std::count(task_ops::tasks),
      avg_priority = (
        task_ops::tasks | std::map(t -> std::to_int64(t.priority)) | std::mean()
      ),
    }
    "
    )
}

#[test]
fn format_10() {
    assert_snapshot!(_format(r#"
func a() ->
 if
 true
 then
 "a"
 else
 "no a"
    "#), @r#"func a() -> if true then "a" else "no a""#
    )
}

#[test]
fn format_11() {
    assert_snapshot!(_format(r#"
func some_very_long_name_that_makes_the_line_break() ->
 if
 true
 then
 "a"
 else
 "no a"
    "#), @r#"
    func some_very_long_name_that_makes_the_line_break() -> if true then (
      "a"
    ) else (
      "no a"
    )
    "#
    )
}

#[test]
fn format_12() {
    // don't duplicate the block parenthesis
    assert_snapshot!(_format(r#"
    func some_very_long_name_that_makes_the_line_break() -> if true then (
      "a"
    ) else (
      "no a"
    )
    "#), @r#"
    func some_very_long_name_that_makes_the_line_break() -> if true then (
      "a"
    ) else (
      "no a"
    )
    "#
    )
}

#[test]
fn format_13() {
    // func calls with single arg

    assert_snapshot!(_format(r#"
    func main() -> my_func([
        something_long_so_the_array_must_be_split,
        over_multiple_lines,
        but_the_func_call_is_not,
    ])
    "#), @r#"
    func main() -> my_func([
      something_long_so_the_array_must_be_split,
      over_multiple_lines,
      but_the_func_call_is_not,
    ])
    "#
    )
}

#[test]
fn format_14() {
    // binary expr that span multiple lines

    assert_snapshot!(_format(r#"
    func main() -> some_long_value
    | some_function_that_has_a_long_name | another_function
    "#), @r#"
    func main() -> (
      some_long_value | some_function_that_has_a_long_name | another_function
    )
    "#
    )
}

#[test]
fn format_15() {
    // binary expr that span multiple lines

    assert_snapshot!(_format(r#"
    func main() -> some_long_value
    | function_that_has_a_long_name | another_function_that_has_even_long_name
    "#), @r#"
    func main() -> (
      some_long_value
      | function_that_has_a_long_name
      | another_function_that_has_even_long_name
    )
    "#
    )
}

#[test]
fn format_16() {
    // don't inject parenthesis in single lines

    assert_snapshot!(_format(r#"
    func get_song_by_id() -> (from_songs | find(aaaaaaa == bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb) | or_default)
    "#), @"
    func get_song_by_id() -> (
      from_songs | find(aaaaaaa == bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb) | or_default
    )
    "
    )
}

#[test]
fn format_17() {
    // formatting of external functions

    assert_snapshot!(_format(r#"
    external func index(array: [T], position: Int64): enum { None, Some: T }
    where T
    "#), @"
    external func index(array: [T], position: Int64): enum {None, Some: T}
    where T
    "
    )
}

#[test]
fn format_18() {
    // variant

    assert_snapshot!(_format(r#"
    const a = .none
    const b = .some(
      a_value_that_is_long)
    "#), @"
    const a = .none

    const b = .some(a_value_that_is_long)
    "
    )
}

#[test]
fn format_19() {
    // literals

    assert_snapshot!(_format(r#"
    const a = 1_000
    const b = 0xff_de
    const c = 10e+10
    "#), @"
    const a = 1_000

    const b = 0xff_de

    const c = 10e+10
    "
    )
}

#[test]
fn format_20() {
    // pipelines

    assert_snapshot!(_format(r#"
    func main() -> (
      my_const
      | my_function_with_a_really_long_name_so_it_cannot_be_single_line(
          arg1, arg2, arg3,
        )
    )
    "#), @"
    func main() -> (
      my_const
      | my_function_with_a_really_long_name_so_it_cannot_be_single_line(
          arg1, arg2, arg3,
        )
    )
    "
    )
}

#[test]
fn format_21() {
    assert_snapshot!(_format(r#"
    import foo::*
    import foo::(baz, bar)
    const   x=1
    "#), @"
    import foo::*
    import foo::(bar, baz)

    const x = 1
    ")
}

#[test]
fn trivia_00() {
    assert_snapshot!(_format(r#"
    # TODO
    const x = [10]
    "#),
    @r"
    # TODO
    const x = [10]
    "
    )
}

#[test]
fn trivia_01() {
    assert_snapshot!(_format(r#"
    const x = [
      # first
      1,
      # second
      2,
    ]
    "#), @r"
    const x = [
      # first
      1,
      # second
      2,
    ]
    "
    )
}

#[test]
fn trivia_02() {
    assert_snapshot!(_format(r#"
    const x = [


      # first


      1,


      # second


      2,


    ]
    "#), @r"
    const x = [

      # first

      1,

      # second

      2,
    ]
    "
    )
}

#[test]
fn trivia_03() {
    assert_snapshot!(_format(r#"
    const x = [
       # first before
      # first before 2
         1, # first after

       # second before
      # second before 2
        2, # second after
    ]
    "#), @r"
    const x = [
      # first before
      # first before 2
      1, # first after

      # second before
      # second before 2
      2, # second after
    ]
    "
    )
}

#[test]
fn trivia_04() {
    assert_snapshot!(_format(r#"
    const x = [ # here
      1, 2, 3
    ]
    "#), @"
    const x = [
      # here
      1, 2, 3,
    ]
    "
    )
}

#[test]
fn trivia_05() {
    assert_snapshot!(_format(r#"
    # leading

    module db {

      # leading sub

      const x = 4 # inline

      # leading sub 2

      const y = 4 # inline 2

      # trailing sub

    }

    # trailing

    "#), @r"
    # leading

    module db {
      # leading sub

      const x = 4 # inline

      # leading sub 2

      const y = 4 # inline 2

      # trailing sub
    }

    # trailing
    "
    )
}

#[test]
fn trivia_06() {
    assert_snapshot!(_format(r#"
    const x = {

        # leading a
       a = { # leading b
         # leading b
         b = true # inline b
         # trailing b
     }, # inline a

         # trailing a
    }       # inline x
    "#), @"
    const x = {

      # leading a
      a = {
        # leading b
        # leading b
        b = true, # inline b

        # trailing b
      }, # inline a

      # trailing a
    } # inline x
    "
    )
}

#[test]
fn trivia_07() {
    assert_snapshot!(_format(r#"
    type Status: enum {
      # initial
      Pending,
      # user has clicked "start"
      InProgress: {started: Text, owner: Text},

      # completed
      Done,
    }
    "#), @r#"
    type Status: enum {
      # initial
      Pending,
      # user has clicked "start"
      InProgress: {started: Text, owner: Text},

      # completed
      Done,
    }
    "#
    );
}

#[test]
fn trivia_08() {
    assert_snapshot!(_format(r#"
    type Task: {
      # id
      Int64,
      # some title
      title: Text,

      # completed
      Bool,

      # TODO: add a few others
    }
 "#), @"
    type Task: {
      # id
      Int64,
      # some title
      title: Text,

      # completed
      Bool,

      # TODO: add a few others
    }
    ");
}

#[test]
fn trivia_09() {
    assert_snapshot!(_format(r#"
    ## A doc comment
    const a = true # trailing
    # one

    # two

    # three
    ## B doc comment
    # four
    const b = true # trailing
    # five
 "#), @r"
    ## A doc comment
    const a = true # trailing

    # one

    # two

    # three
    ## B doc comment
    # four
    const b = true # trailing

    # five
    ");
}

#[test]
fn trivia_10() {
    assert_snapshot!(_format(r#"
# Utils

external func default(): T
where T
 "#), @r"
    # Utils

    external func default(): T
    where T
    ");
}

#[test]
fn trivia_11() {
    assert_snapshot!(_format(r#"
    ## This is a def
    ## with a triple
    ## doc comment
    const x = false
 "#), @r"
    ## This is a def
    ## with a triple
    ## doc comment
    const x = false
    ");
}

#[test]
fn trivia_12() {
    assert_snapshot!(_format(r#"
    func main() -> (
      # this is a
      let a = false; # trailing a

      # this is b
      let b = false; # trailing b

      # this is main
      true
    )
 "#), @r"
    func main() -> (
      # this is a
      let a = false; # trailing a

      # this is b
      let b = false; # trailing b

      # this is main
      true
    )
    ");
}

#[test]
fn source_00() {
    assert_snapshot!(_format(r"submodule const a = 3"), @r"
    submodule

    const a = 3
    ");
}

#[test]
fn source_01() {
    // When a def fails to print, don't produce an edit for it.
    // (for tests, we mark missing nodes with `...`)

    assert_snapshot!(_format(r#"
const a = 3

## Comment
# TODO: make this shorter
const a_name_that_is_too_long_to_fit_on_one_line_and_will_surly_fail_to_print = 4 # TODO: too long

const b = 5
    "#), @r"
    const a = 3

    ## Comment
    # TODO: make this shorter
    const a_name_that_is_too_long_to_fit_on_one_line_and_will_surly_fail_to_print = 4 # TODO: too long

    const b = 5
    ");
}

#[test]
fn source_02() {
    // framed type defs

    assert_snapshot!(_format(r#"
    type Date(Int32)
    "#), @"type Date(Int32)");
}

#[test]
fn source_03() {
    // annotations

    assert_snapshot!(_format(r#"
    # before
    ## Doc comment
    # after doc
    @Debug("Clone", "Debug")
    # after annotation
    type Date: Int32 # trailing

    # after
    "#), @r#"
    # before
    ## Doc comment
    # after doc
    @Debug("Clone", "Debug")
    # after annotation
    type Date: Int32 # trailing

    # after
    "#);
}

// ── format_def_signature ────────────────────────────────────────────────────

#[track_caller]
fn _signature(source: &str) -> String {
    use super::print_def_signature;
    use crate::check;
    use crate::project::SourceTree;

    let tree = SourceTree::single("".into(), source.to_string());
    let project = check(tree, Default::default()).expect("type error");
    let (name, def) = (project.root_module.iter_defs())
        .find(|(n, _)| *n != crate::resolver::NS_STD)
        .unwrap();
    print_def_signature(name, def).expect("no signature")
}

#[test]
fn signature_function() {
    assert_snapshot!(_signature(
        "func greet(name: Text): Text -> name",
    ), @"func greet(Text): Text");
}

#[test]
fn signature_function_no_return() {
    assert_snapshot!(_signature(
        "func identity(x: Int32) -> x",
    ), @"func identity(Int32): Int32");
}

#[test]
fn signature_const() {
    assert_snapshot!(_signature(
        "const answer: Int32 = 42",
    ), @"const answer: Int32 = 42");
}

#[test]
fn signature_type() {
    assert_snapshot!(_signature(
        "type Point: { x: Float64, y: Float64 }",
    ), @"type Point: {x: Float64, y: Float64}");
}

#[test]
fn signature_type_doc_comment() {
    // doc_comment is not part of the signature — it belongs to the hover
    // Markdown, not the code block; verify the signature alone is clean.
    assert_snapshot!(_signature(r#"
        ## A named point in 2-D space.
        type Point: { x: Float64, y: Float64 }
    "#), @"type Point: {x: Float64, y: Float64}");
}

#[test]
fn signature_generic_function() {
    // Generic functions have a `where` clause that must appear on a new line
    // in the hover signature. This test pins the exact multiline output so
    // regressions in the where-clause rendering are caught.
    assert_snapshot!(_signature(
        "func identity(x: T): T where T -> x",
    ), @"
    func identity(T): T
    where T
    ");
}

#[test]
fn signature_number_domain() {
    // `where T: number` must round-trip as `number`, not the full list of types.
    assert_snapshot!(_signature(
        "func double(x: T): T where T: number -> x",
    ), @"
    func double(T): T
    where T: number
    ");
}

#[test]
fn signature_primitive_domain() {
    // `where T: primitive` must round-trip as `primitive`, not the full list.
    assert_snapshot!(_signature(
        "func identity(x: T): T where T: primitive -> x",
    ), @"
    func identity(T): T
    where T: primitive
    ");
}

#[test]
fn signature_partial_number_domain() {
    // A custom subset that is NOT the canonical `number` set must print verbatim.
    assert_snapshot!(_signature(
        "func negi(x: T): T where T: Int8 | Int16 | Int32 | Int64 -> x",
    ), @"
    func negi(T): T
    where T: Int8 | Int16 | Int32 | Int64
    ");
}

#[test]
fn emits_definition_granular_edits() {
    let source = "const   a=1\nconst   b=2\nconst   c=3";
    let (parsed, edits) = _print_edits(source);

    let def_spans: Vec<_> = parsed
        .root
        .defs
        .iter()
        .map(|(_, def)| def.span.expect("expected span"))
        .collect();

    for def_span in def_spans {
        let covering = edits
            .iter()
            .filter(|e| e.span.start <= def_span.start && def_span.end() <= e.span.end())
            .count();
        assert_eq!(
            covering, 1,
            "expected exactly one edit to cover definition at {:?}",
            def_span
        );
    }
}

#[test]
fn minimize_drops_unchanged_definition_edits() {
    let source = "const a = 1\nconst   b=2\n";
    let (_parsed, edits) = _print_edits(source);

    let minimized = crate::codespan::minimize_text_edits(source, edits);
    assert_eq!(minimized.len(), 1);

    let out = crate::codespan::apply_text_edits(source, &minimized);
    assert_eq!(out, "const a = 1\n\nconst b = 2\n");
}
