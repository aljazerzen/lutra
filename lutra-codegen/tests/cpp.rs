//! These tests require a C++20 compiler (`CXX`, or `c++` on PATH).
//! No fixture bytes are hand-written: the checked compiler schema and Lutra's
//! dynamic Value encoder/decoder are the reference for both wire directions.
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

use lutra_bin::{Decode, Encode, Value, ir, rr};
use lutra_codegen::{Error, GenerateOptions, ProgramRepr, Target, generate};
use lutra_compiler::{CompileParams, Project, SourceTree};

struct Scratch(PathBuf);

impl Scratch {
    fn new() -> Self {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        loop {
            let path = std::env::temp_dir().join(format!(
                "lutra-cpp-{}-{}",
                std::process::id(),
                NEXT.fetch_add(1, Ordering::Relaxed)
            ));
            match fs::create_dir(&path) {
                Ok(()) => return Self(path),
                Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => continue,
                Err(e) => panic!("create {}: {e}", path.display()),
            }
        }
    }

    fn path(&self, name: &str) -> PathBuf {
        self.0.join(name)
    }

    fn header(&self, project: &Project, options: GenerateOptions) -> String {
        let path = self.path("generated.hpp");
        generate(project, Target::Cpp, &path, options).unwrap();
        fs::read_to_string(path).unwrap()
    }

    fn run_cpp(&self, source: &str) {
        fs::write(self.path("test.cpp"), source).unwrap();
        compile_and_run(&self.path("test.cpp"), &self.0);
    }
}

impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn checked_command(command: &mut Command) -> Output {
    let output = command
        .output()
        .unwrap_or_else(|e| panic!("{command:?}: {e}"));
    assert!(
        output.status.success(),
        "{command:?}: {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    output
}

fn compile_and_run(source: &Path, directory: &Path) {
    let exe = directory.join(format!("test{}", std::env::consts::EXE_SUFFIX));
    checked_command(
        Command::new(std::env::var_os("CXX").unwrap_or_else(|| "c++".into()))
            .args(["-std=c++20", "-Wall", "-Wextra", "-Werror", "-pedantic"])
            .arg(source)
            .arg("-o")
            .arg(&exe),
    );
    checked_command(Command::new(exe).current_dir(directory));
}

fn project(source: &str) -> Project {
    lutra_compiler::check(
        SourceTree::single("".into(), source.to_owned()),
        Default::default(),
    )
    .unwrap_or_else(|e| panic!("{e}"))
}

fn definitions(project: &Project) -> Vec<ir::TyDef> {
    lutra_compiler::project_to_types(project)
        .iter_types_re()
        .map(|(name, ty)| ir::TyDef {
            name,
            ty: ty.clone(),
        })
        .collect()
}

const IO: &str = r#"
#include "generated.hpp"
#include <cassert>
#include <fstream>
#include <iterator>
#include <type_traits>
namespace g = lutra_generated;
std::vector<std::uint8_t> read(const char* path) {
  std::ifstream file(path, std::ios::binary);
  assert(file.good());
  return {std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>()};
}
template<class Bytes> void write(const char* path, const Bytes& bytes) {
  std::ofstream file(path, std::ios::binary);
  file.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
  assert(file.good());
}
template<class T> void interop(const char* input, const char* output, const T& expected) {
  const auto wire = read(input);
  const auto decoded = lutra_cpp::decode<T>(wire);
  // Decode Rust bytes and compare to a separately constructed native value.
  assert(lutra_cpp::encode(decoded) == lutra_cpp::encode(expected));
  // Write the independent native value, not just the bytes read from Rust.
  write(output, lutra_cpp::encode(expected));
}
"#;

/// Each case is (Lutra type path, dynamic Rust value, independent C++ value).
fn interop(source: &str, cases: &[(&str, Value, &str)]) {
    let project = project(source);
    let defs = definitions(&project);
    let scratch = Scratch::new();
    scratch.header(&project, GenerateOptions::default());
    let mut cpp = format!("{IO}\nint main() {{\n");
    for (i, (name, value, expression)) in cases.iter().enumerate() {
        let ty = &defs
            .iter()
            .find(|d| d.name.0.join("::") == *name)
            .unwrap()
            .ty;
        fs::write(
            scratch.path(&format!("in{i}")),
            value.encode(ty, &defs).unwrap(),
        )
        .unwrap();
        cpp.push_str(&format!("interop(\"in{i}\", \"out{i}\", {expression});\n"));
    }
    cpp.push_str("}\n");
    scratch.run_cpp(&cpp);
    for (i, (name, value, _)) in cases.iter().enumerate() {
        let ty = &defs
            .iter()
            .find(|d| d.name.0.join("::") == *name)
            .unwrap()
            .ty;
        let bytes = fs::read(scratch.path(&format!("out{i}"))).unwrap();
        assert_eq!(
            Value::decode(&bytes, ty, &defs).unwrap(),
            *value,
            "case {i}: {name}"
        );
        assert_eq!(
            bytes,
            value.encode(ty, &defs).unwrap(),
            "canonical bytes for case {i}: {name}"
        );
    }
}

fn new_text(value: &str) -> Value {
    Value::new_text(value)
}

fn unit() -> Value {
    Value::unit()
}

fn variant(tag: usize, value: Value) -> Value {
    Value::Enum(tag, Box::new(value))
}

#[cfg(feature = "clap")]
#[test]
fn cli_defaults_allow_cpp_generation() {
    use clap::Parser;
    #[derive(Parser)]
    struct Cli {
        #[command(flatten)]
        options: GenerateOptions,
    }
    let cli = Cli::try_parse_from(["gen-code"]).unwrap();
    assert!(!cli.options.function_traits);
    let scratch = Scratch::new();
    scratch.header(&project("type Record: {Int32}"), cli.options);
    for flag in ["--function-traits", "--no-function-traits"] {
        assert!(
            Cli::try_parse_from(["gen-code", flag])
                .unwrap()
                .options
                .function_traits
        );
    }
}

#[test]
fn standalone_runtime() {
    let scratch = Scratch::new();
    compile_and_run(
        &Path::new(env!("CARGO_MANIFEST_DIR")).join("src/cpp/runtime_test.cpp"),
        &scratch.0,
    );
}

#[test]
fn tuples_text_arrays_modules_aliases_and_escaped_identifiers() {
    use Value::*;
    interop(
        r#"
        module models {
          type Record: {class: Text, Int32, flags: [Bool], nested: [[Text]], unit: {}}
          type Alias: Record
        }
        type Bundle: {models::Alias, pair: {Uint16, Text}}
        type class: {class: Uint8, value: Int16, lutra_id_636c617373: Text}
        type Mixed: {field1: Uint8, Uint16}
        type Empty: {}
        type Units: [{}]
        "#,
        &[
            (
                "Bundle",
                Tuple(vec![
                    Tuple(vec![
                        new_text("hé\0llo"),
                        Prim32((-123i32) as u32),
                        Array(vec![Prim8(0), Prim8(1), Prim8(0)]),
                        Array(vec![
                            Array(vec![new_text("a"), new_text("β")]),
                            Array(vec![]),
                        ]),
                        unit(),
                    ]),
                    Tuple(vec![Prim16(65535), new_text("")]),
                ]),
                r#"g::Bundle{g::models::Alias{std::string("h\xc3\xa9\0llo", 7), -123, {false, true, false}, {{"a", "\xce\xb2"}, {}}, {}}, {65535, ""}}"#,
            ),
            (
                "class",
                Tuple(vec![Prim8(42), Prim16((-9i16) as u16), new_text("escaped")]),
                r#"g::lutra_id_636c617373{42, -9, "escaped"}"#,
            ),
            ("Mixed", Tuple(vec![Prim8(1), Prim16(2)]), "g::Mixed{1, 2}"),
            ("Empty", unit(), "g::Empty{}"),
            ("Units", Array(vec![unit(), unit(), unit()]), "g::Units(3)"),
        ],
    );
}

#[test]
fn inline_and_indirect_enum_wire_layouts() {
    use Value::*;
    interop(
        r#"
        type Inline: enum {empty, small: Uint8, big: Uint16}
        type Indirect: enum {empty, number: Int64, text: Text, tuple: {Text, [Int32]}}
        type Both: {[Inline], [Indirect]}
        "#,
        &[
            (
                "Inline",
                variant(0, unit()),
                "g::Inline{g::Inline::empty{}}",
            ),
            (
                "Inline",
                variant(1, Prim8(255)),
                "g::Inline{g::Inline::small{255}}",
            ),
            (
                "Inline",
                variant(2, Prim16(60000)),
                "g::Inline{g::Inline::big{60000}}",
            ),
            (
                "Indirect",
                variant(0, unit()),
                "g::Indirect{g::Indirect::empty{}}",
            ),
            (
                "Indirect",
                variant(1, Prim64((-123456789i64) as u64)),
                "g::Indirect{g::Indirect::number{-123456789}}",
            ),
            (
                "Indirect",
                variant(2, new_text("hello")),
                r#"g::Indirect{g::Indirect::text{"hello"}}"#,
            ),
            (
                "Indirect",
                variant(
                    3,
                    Tuple(vec![
                        new_text("nested"),
                        Array(vec![Prim32(1), Prim32(u32::MAX)]),
                    ]),
                ),
                r#"g::Indirect{g::Indirect::tuple{{"nested", {1, -1}}}}"#,
            ),
            (
                "Both",
                Tuple(vec![
                    Array(vec![variant(0, unit()), variant(2, Prim16(513))]),
                    Array(vec![
                        variant(2, new_text("x")),
                        variant(0, unit()),
                        variant(1, Prim64(9)),
                    ]),
                ]),
                r#"g::Both{{g::Inline{g::Inline::empty{}}, g::Inline{g::Inline::big{513}}}, {g::Indirect{g::Indirect::text{"x"}}, g::Indirect{g::Indirect::empty{}}, g::Indirect{g::Indirect::number{9}}}}"#,
            ),
        ],
    );
}

#[test]
fn malformed_generated_enum_and_tuple_data_is_rejected() {
    let project = project(
        r#"
        type Inline: enum {empty, small: Uint16}
        type Indirect: enum {empty, text: Text}
        type Record: {Text, [Int32]}
    "#,
    );
    let scratch = Scratch::new();
    scratch.header(&project, GenerateOptions::default());
    scratch.run_cpp(&format!(
        r#"{IO}
    template<class F> void rejects(F action) {{
      bool rejected = false;
      try {{ action(); }} catch (const lutra_cpp::error&) {{ rejected = true; }}
      assert(rejected);
    }}
    template<class T> void truncations(const T& value) {{
      const auto bytes = lutra_cpp::encode(value);
      for (std::size_t n = 0; n < bytes.size(); ++n)
        rejects([&] {{ (void)lutra_cpp::decode<T>(std::span(bytes).first(n)); }});
    }}
    int main() {{
      truncations(g::Inline{{g::Inline::small{{513}}}});
      truncations(g::Indirect{{g::Indirect::text{{"abc"}}}});
      truncations(g::Record{{"abc", {{1, 2, 3}}}});
      auto direct = lutra_cpp::encode(g::Inline{{g::Inline::small{{1}}}});
      direct[0] = 255;
      rejects([&] {{ (void)lutra_cpp::decode<g::Inline>(direct); }});
      auto indirect = lutra_cpp::encode(g::Indirect{{g::Indirect::text{{"abc"}}}});
      indirect[0] = 255;
      rejects([&] {{ (void)lutra_cpp::decode<g::Indirect>(indirect); }});
      indirect[0] = 1;
      for (int i = 1; i <= 4; ++i) indirect[i] = 255;
      rejects([&] {{ (void)lutra_cpp::decode<g::Indirect>(indirect); }});
    }}
    "#
    ));
}

#[test]
fn escaped_names_remain_distinct_and_accessible() {
    let project = project(
        r#"
        module class {
          type Record: {class: Uint8, lutra_id_636c617373: Uint8, Record: Uint8, value: Uint8}
          type Choice: enum {class: Uint8, Choice: Uint8, value}
        }
        type Mixed: {field1: Uint8, Uint16}
    "#,
    );
    let scratch = Scratch::new();
    scratch.header(&project, GenerateOptions::default());
    scratch.run_cpp(
        r#"
        #include "generated.hpp"
        #include <cassert>
        namespace g = lutra_generated;
        int main() {
          g::lutra_id_636c617373::Record record{1, 2, 3, 4};
          assert(record.lutra_id_636c617373 == 1);
          assert(record.lutra_id_6c757472615f69645f36333663363137333733 == 2);
          assert(record.lutra_member_5265636f7264 == 3);
          assert(record.lutra_id_76616c7565 == 4);
          using Choice = g::lutra_id_636c617373::Choice;
          Choice c{Choice::lutra_member_43686f696365{9}};
          auto decoded = lutra_cpp::decode<Choice>(lutra_cpp::encode(c));
          assert(std::get<Choice::lutra_member_43686f696365>(decoded.value).value == 9);
          g::Mixed mixed{5, 6};
          assert(mixed.field1 == 5);
          assert(mixed.lutra_field_1 == 6);
        }
    "#,
    );
}

#[test]
fn standard_scalar_and_temporal_types() {
    use Value::*;
    interop(
        r#"
        type Scalars: {Bool, Int8, Int16, Int32, Int64, Uint8, Uint16, Uint32, Uint64, Float32, Float64}
        type Temporal: {Date, Duration, Time, Timestamp, Decimal}
        "#,
        &[
            (
                "Scalars",
                Tuple(vec![
                    Prim8(1),
                    Prim8(i8::MIN as u8),
                    Prim16(i16::MIN as u16),
                    Prim32(i32::MIN as u32),
                    Prim64(i64::MIN as u64),
                    Prim8(u8::MAX),
                    Prim16(u16::MAX),
                    Prim32(u32::MAX),
                    Prim64(u64::MAX),
                    Prim32(1.25f32.to_bits()),
                    Prim64((-0.0f64).to_bits()),
                ]),
                "g::Scalars{true, INT8_MIN, INT16_MIN, INT32_MIN, INT64_MIN, UINT8_MAX, UINT16_MAX, UINT32_MAX, UINT64_MAX, 1.25f, -0.0}",
            ),
            (
                "Temporal",
                Tuple(vec![
                    Prim32((-20i32) as u32),
                    Prim64((-42i64) as u64),
                    Prim64(123456),
                    Prim64(987654321),
                    Prim64(0x123456789abcdef0),
                ]),
                "g::Temporal{{-20}, {-42}, {123456}, {987654321}, {UINT64_C(0x123456789abcdef0)}}",
            ),
        ],
    );
}

#[test]
fn generated_programs_are_serialized_runner_programs() {
    let project = project(
        r#"
        func root(x: Int32) -> x
        module api {
          func echo(x: Text) -> x
          module nested { func identity(x: Int64) -> x }
        }
        module excluded { func hidden(x: Int32) -> x }
    "#,
    );
    let scratch = Scratch::new();
    let options = GenerateOptions::default()
        .generate_programs("", ProgramRepr::BytecodeLt, [])
        .generate_programs("api", ProgramRepr::BytecodeLt, [])
        .generate_programs("api::nested", ProgramRepr::BytecodeLt, []);
    let header = scratch.header(&project, options.clone());
    assert!(!header.contains("struct hidden"));
    scratch.run_cpp(&format!(
        r#"{IO}
int main() {{
  static_assert(std::is_same_v<g::api::echo::output_type, std::string>);
  static_assert(std::is_same_v<g::root::output_type, std::int32_t>);
  [[maybe_unused]] g::api::echo::input_type input{{"hello"}};
  write("root", g::root::bytes);
  write("echo", g::api::echo::bytes);
  write("identity", g::api::nested::identity::bytes);
}}
"#
    ));
    for (path, file) in [
        ("root", "root"),
        ("api::echo", "echo"),
        ("api::nested::identity", "identity"),
    ] {
        let bytes = fs::read(scratch.path(file)).unwrap();
        let decoded = rr::Program::decode(&bytes).unwrap();
        let (expected, _) =
            lutra_compiler::compile(&project, &CompileParams::new(path, ProgramRepr::BytecodeLt))
                .unwrap();
        assert_eq!(decoded.encode(), expected.encode());
        assert_eq!(bytes, expected.encode());
        assert!(decoded.into_bytecode_lt().is_ok());
    }
    // Hash-map insertion order and output file location must not affect output.
    let reordered = GenerateOptions::default()
        .generate_programs("api::nested", ProgramRepr::BytecodeLt, [])
        .generate_programs("api", ProgramRepr::BytecodeLt, [])
        .generate_programs("", ProgramRepr::BytecodeLt, []);
    assert_eq!(header, scratch.header(&project, reordered));
    let second = Scratch::new();
    assert_eq!(header, second.header(&project, options));
}

#[test]
fn suppression_flags_produce_compilable_headers() {
    let project = project("type Record: {Text, Int32}\nfunc echo(x: Record) -> x");
    for no_types in [false, true] {
        for no_codecs in [false, true] {
            let scratch = Scratch::new();
            let options = GenerateOptions {
                no_types,
                no_encode_decode: no_codecs,
                ..GenerateOptions::default().generate_programs("", ProgramRepr::BytecodeLt, [])
            };
            let header = scratch.header(&project, options);
            assert_eq!(header.contains("struct Record {"), !no_types);
            assert_eq!(header.contains("using input_type ="), !no_types);
            assert_eq!(
                header.contains("template <> struct codec<::lutra_generated::Record>"),
                !no_types && !no_codecs
            );
            assert!(header.contains("struct echo {"));
            scratch.run_cpp("#include \"generated.hpp\"\n#include \"generated.hpp\"\nint main() { return lutra_generated::echo::bytes.empty(); }\n");
        }
    }
}

#[test]
fn unsupported_options_return_errors_without_overwriting_output() {
    let project = project("type Record: {Int32}");
    let scratch = Scratch::new();
    let path = scratch.path("generated.hpp");
    for options in [
        GenerateOptions::default().function_traits(),
        GenerateOptions::default().generate_client(),
        GenerateOptions::default().no_std(),
        GenerateOptions::default().with_lutra_bin_path("::custom_bin".into()),
        GenerateOptions::default().with_lutra_runner_path("::custom_runner".into()),
    ] {
        fs::write(&path, "keep me").unwrap();
        let error = generate(&project, Target::Cpp, &path, options).unwrap_err();
        assert!(matches!(error, Error::Unsupported(_)), "{error}");
        assert_eq!(fs::read_to_string(&path).unwrap(), "keep me");
    }
}

#[test]
fn function_valued_types_are_explicitly_rejected() {
    let project = project("type Callback: func (x: Int32): Int32");
    let scratch = Scratch::new();
    let error = generate(
        &project,
        Target::Cpp,
        scratch.path("generated.hpp"),
        GenerateOptions::default(),
    )
    .unwrap_err();
    assert!(
        matches!(&error, Error::Unsupported(message) if message.contains("function-valued")),
        "{error}"
    );
    assert!(!scratch.path("generated.hpp").exists());
}

#[test]
fn nested_function_valued_types_are_explicitly_rejected() {
    for source in [
        "type Record: {callback: func (x: Int32): Int32}",
        "type Callbacks: [func (x: Int32): Int32]",
        "type Choice: enum {none, callback: func (x: Int32): Int32}",
        "module nested { type Record: {callbacks: [func (x: Int32): Int32]} }",
        "type Callback: func (x: Int32): Int32\ntype Record: {callback: Callback}",
    ] {
        let project = project(source);
        let scratch = Scratch::new();
        let path = scratch.path("generated.hpp");
        fs::write(&path, "keep me").unwrap();
        let error = generate(&project, Target::Cpp, &path, GenerateOptions::default()).unwrap_err();
        assert!(
            matches!(&error, Error::Unsupported(message) if message.contains("function-valued")),
            "{source}: {error}"
        );
        assert_eq!(fs::read_to_string(&path).unwrap(), "keep me");
    }
}

#[test]
fn io_errors_are_returned() {
    let project = project("type Record: {Int32}");
    let scratch = Scratch::new();
    let error = generate(
        &project,
        Target::Cpp,
        scratch.path("missing/generated.hpp"),
        GenerateOptions::default(),
    )
    .unwrap_err();
    assert!(matches!(error, Error::Io(_)), "{error}");
}

#[test]
fn recursive_types_are_explicitly_rejected() {
    let project = project("type Tree: enum {Leaf: Uint8, Node: {left: Tree, right: Tree}}");
    let scratch = Scratch::new();
    let error = generate(
        &project,
        Target::Cpp,
        scratch.path("generated.hpp"),
        GenerateOptions::default(),
    )
    .unwrap_err();
    assert!(
        matches!(&error, Error::Unsupported(message) if message.contains("recursive")),
        "{error}"
    );
    assert!(!scratch.path("generated.hpp").exists());
}
