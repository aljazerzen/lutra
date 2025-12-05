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
    @r"
    const long_name = [
      hello::world, hello::world, hello::world, hello::world, hello::world
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
    "#), @r"
    const x = [
      [hello::world, hello::world, hello::world, hello::world, hello::world]
    ]
    "
    )
}

#[test]
fn format_05() {
    assert_snapshot!(_format(r#"
    const x = [[hello::world, hello::world, hello::world], [hello::world, hello::world]]
    "#), @r"
    const x = [
      [hello::world, hello::world, hello::world], [hello::world, hello::world]
    ]
    "
    )
}

#[test]
fn format_06() {
    assert_snapshot!(_format(r#"
    const x = [hello::world, [hello::world, hello::world, hello::world], hello::world]
    "#), @r"
    const x = [
      hello::world, [hello::world, hello::world, hello::world], hello::world
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
    | std::map(func (t) -> std::to_int64(t.priority))
    | std::average()
  ),
}
    "#), @r"
    const stats = {
      total_tasks = std::count(task_ops::tasks),
      avg_priority = (
        task_ops::tasks
        | std::map(func (t) -> std::to_int64(t.priority))
        | std::average()
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
    "#), @r"
    const x = [
      # here
      1, 2, 3
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
    "#), @r"
    const x = {

      # leading a
      a = {
        # leading b
        # leading b
        b = true # inline b

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
      InProgress: {started: text, owner: text},

      # completed
      Done,
    }
    "#), @r#"
    type Status: enum {
      # initial
      Pending,
      # user has clicked "start"
      InProgress: {started: text, owner: text},

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
      int64,
      # some title
      title: text,

      # completed
      bool,

      # TODO: add a few others
    }
 "#), @r"
    type Task: {
      # id
      int64,
      # some title
      title: text,

      # completed
      bool,

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

func default(): T
where T
 "#), @r"
    # Utils

    func default(): T
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
    // nominal type defs

    assert_snapshot!(_format(r#"
    type Date(int32)
    "#), @r"
    type Date(int32)
    ");
}
