#![cfg(test)]

use insta::assert_snapshot;

#[track_caller]
fn _format(source: &str) -> String {
    let (ast, dia, trivia) = crate::parser::parse_source(source, 0);

    let Some(ast) = ast else {
        panic!("parse err: {dia:?}");
    };

    super::print_source(&ast, Some(&trivia))
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
