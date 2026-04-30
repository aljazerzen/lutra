use std::path::{Path, PathBuf};

use lutra_bin::{Value, ir, rr};

use crate::shell::Shell;
use crate::terminal::{ActionResult, View};

pub fn run(shell: &mut Shell, input: Vec<u8>) -> ActionResult {
    let request = match decode_request(&input) {
        Ok(request) => request,
        Err(RequestError::Validation(err)) => {
            if let Some(input) = &mut shell.repl.draft.input {
                input.set_error(err);
            }
            return ActionResult::redraw();
        }
        Err(RequestError::Internal(err)) => return shell.repl.commit_error(err),
    };

    let Some(output) = shell.repl.last_successful_output() else {
        return shell.repl.commit_error("no result to export");
    };

    let project_dir = shell
        .project
        .source
        .as_ref()
        .map(|source| source.get_project_dir());
    let request = Input {
        format: request.format,
        path: resolve_export_path(&request.path, project_dir),
    };

    match write_fs(&request, output) {
        Ok(()) => shell.repl.commit_message(
            View::from_text(&format!("Written: {}", request.path.display())).to_owned(),
        ),
        Err(err) => shell.repl.commit_error(err.to_string()),
    }
}

pub fn program_ty() -> rr::ProgramType {
    rr::ProgramType {
        input: ir::Ty::new(vec![
            ir::TyTupleField {
                name: Some("format".into()),
                ty: ir::Ty::new(ir::TyKind::Enum(vec![
                    ir::TyEnumVariant {
                        name: "LutraSource".into(),
                        ty: ir::Ty::new_unit(),
                    },
                    ir::TyEnumVariant {
                        name: "RawBinary".into(),
                        ty: ir::Ty::new_unit(),
                    },
                ])),
            },
            ir::TyTupleField {
                name: Some("path".into()),
                ty: ir::Ty::new(ir::TyPrimitive::text),
            },
        ]),
        output: ir::Ty::new_unit(),
        defs: Default::default(),
    }
}

pub fn default_input() -> Value {
    Value::Tuple(vec![
        Value::Enum(0, Box::new(Value::unit())),
        Value::Text(Format::LutraSource.default_path().into()),
    ])
}

pub struct ProgramOutput<'a> {
    pub bytes: &'a [u8],
    pub program_ty: &'a rr::ProgramType,
}

pub struct Input {
    pub format: Format,
    pub path: PathBuf,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Format {
    LutraSource,
    RawBinary,
}

impl Format {
    pub fn default_path(self) -> &'static str {
        match self {
            Format::LutraSource => "export.lt",
            Format::RawBinary => "export.bin",
        }
    }
}

enum RequestError {
    Validation(String),
    Internal(String),
}

fn decode_request(input: &[u8]) -> Result<Input, RequestError> {
    let program_ty = program_ty();
    let value = Value::decode(input, &program_ty.input, &program_ty.defs)
        .map_err(|e| RequestError::Internal(e.to_string()))?;
    let Value::Tuple(fields) = value else {
        return Err(RequestError::Internal(
            "internal error: invalid /export input".to_string(),
        ));
    };
    let [format, path] = fields.as_slice() else {
        return Err(RequestError::Internal(
            "internal error: invalid /export input".to_string(),
        ));
    };

    let format = match format {
        Value::Enum(0, _) => Format::LutraSource,
        Value::Enum(1, _) => Format::RawBinary,
        _ => {
            return Err(RequestError::Internal(
                "internal error: invalid /export format".to_string(),
            ));
        }
    };
    let Value::Text(path) = path else {
        return Err(RequestError::Internal(
            "internal error: invalid /export path".to_string(),
        ));
    };
    let path = path.trim();
    if path.is_empty() {
        return Err(RequestError::Validation(
            "export path cannot be empty".to_string(),
        ));
    }

    Ok(Input {
        format,
        path: PathBuf::from(path),
    })
}

pub fn resolve_export_path(path: &Path, project_dir: Option<&Path>) -> PathBuf {
    if path.is_absolute() {
        path.to_path_buf()
    } else if let Some(project_dir) = project_dir {
        project_dir.join(path)
    } else {
        path.to_path_buf()
    }
}

fn write_fs(request: &Input, output: ProgramOutput<'_>) -> Result<(), anyhow::Error> {
    if let Some(parent) = request.path.parent()
        && !parent.as_os_str().is_empty()
    {
        std::fs::create_dir_all(parent)?;
    }

    match request.format {
        Format::LutraSource => {
            let text = lutra_bin::print_source(
                output.bytes,
                &output.program_ty.output,
                &output.program_ty.defs,
            )
            .map_err(|e| anyhow::anyhow!(e.to_string()))?;
            std::fs::write(&request.path, text)?;
        }
        Format::RawBinary => {
            std::fs::write(&request.path, output.bytes)?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolve_relative_path_against_project_dir() {
        let path = resolve_export_path(Path::new("out.lt"), Some(Path::new("/tmp/project")));
        assert_eq!(path, PathBuf::from("/tmp/project/out.lt"));
    }

    #[test]
    fn keep_absolute_path_unchanged() {
        let path = resolve_export_path(Path::new("/tmp/out.bin"), Some(Path::new("/tmp/project")));
        assert_eq!(path, PathBuf::from("/tmp/out.bin"));
    }
}
