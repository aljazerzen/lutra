use std::sync::Arc;

use lutra_bin::Decode;
use lutra_runner::Run;
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;

/// Main module declaration
#[pymodule(name = "lutra_runner_postgres")]
fn main(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<Runner>()?;
    Ok(())
}

#[pyclass]
struct Runner {
    inner: Arc<lutra_runner_postgres::RunnerAsync>,
}

#[pymethods]
impl Runner {
    #[staticmethod]
    fn connect(py: Python<'_>, config: String) -> PyResult<Bound<'_, PyAny>> {
        pyo3_async_runtimes::tokio::future_into_py(py, async move {
            // TODO: tls options
            let res = tokio_postgres::connect(&config, tokio_postgres::NoTls).await;
            let (client, conn) = res.map_err(|e| PyValueError::new_err(e.to_string()))?;

            tokio::spawn(async move {
                if let Err(e) = conn.await {
                    log::error!("connection error: {e}");
                }
            });

            Ok(Runner {
                inner: Arc::new(lutra_runner_postgres::RunnerAsync::new(client)),
            })
        })
    }

    fn run<'py>(
        slf: &Bound<'py, Self>,
        py: Python<'py>,
        program: Bound<'py, PyAny>,
        input: Bound<'py, PyAny>,
    ) -> PyResult<Bound<'py, PyAny>> {
        let lrp = py.import("lutra_runner_postgres")?;
        let run = lrp.getattr("run")?;
        run.call((slf, program, input), Default::default())
    }

    /// Prepare a program and return its program_id handle.
    fn prepare<'py>(&self, py: Python<'py>, program: &[u8]) -> PyResult<Bound<'py, PyAny>> {
        let program = lutra_bin::rr::Program::decode(program)
            .map_err(|e| PyValueError::new_err(e.to_string()))?;
        let inner = self.inner.clone();

        pyo3_async_runtimes::tokio::future_into_py(py, async move {
            inner
                .prepare(program)
                .await
                .map_err(|e| PyValueError::new_err(e.to_string()))
        })
    }

    /// Execute a prepared program by its program_id handle.
    fn execute<'py>(
        &self,
        py: Python<'py>,
        program_id: u32,
        input: Vec<u8>,
    ) -> PyResult<Bound<'py, PyAny>> {
        let inner = self.inner.clone();

        pyo3_async_runtimes::tokio::future_into_py(py, async move {
            inner
                .execute(program_id, &input)
                .await
                .map_err(|e| PyValueError::new_err(e.to_string()))
        })
    }

    /// Release a prepared program by its program_id handle.
    fn release<'py>(&self, py: Python<'py>, program_id: u32) -> PyResult<Bound<'py, PyAny>> {
        let inner = self.inner.clone();

        pyo3_async_runtimes::tokio::future_into_py(py, async move {
            inner
                .release(program_id)
                .await
                .map_err(|e| PyValueError::new_err(e.to_string()))
        })
    }

    fn pull_schema<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let inner = self.inner.clone();

        pyo3_async_runtimes::tokio::future_into_py(py, async move {
            inner
                .pull_schema()
                .await
                .map_err(|e| PyValueError::new_err(e.to_string()))
        })
    }

    fn shutdown<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let inner = self.inner.clone();

        pyo3_async_runtimes::tokio::future_into_py(py, async move {
            inner
                .shutdown()
                .await
                .map_err(|e| PyValueError::new_err(e.to_string()))
        })
    }
}
