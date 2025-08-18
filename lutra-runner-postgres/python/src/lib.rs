use std::sync::Arc;

use lutra_bin::Decode;
use lutra_runner::Run;
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;

/// Main module declaration
#[pymodule(name = "lutra_runner_postgres")]
fn main(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<Runner>()?;
    m.add_class::<Prepared>()?;
    Ok(())
}

#[pyclass]
struct Runner {
    inner: Arc<lutra_runner_postgres::RunnerAsync>,
}

#[pyclass(frozen)]
struct Prepared {
    inner: lutra_runner_postgres::PreparedProgram,
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

    fn prepare<'py>(&self, py: Python<'py>, program: &[u8]) -> PyResult<Bound<'py, PyAny>> {
        let program = lutra_bin::rr::Program::decode(program)
            .map_err(|e| PyValueError::new_err(e.to_string()))?;
        let inner = self.inner.clone();

        pyo3_async_runtimes::tokio::future_into_py(py, async move {
            let prepared =
                (inner.prepare(program).await).map_err(|e| PyValueError::new_err(e.to_string()))?;

            Ok(Prepared { inner: prepared })
        })
    }

    fn execute<'py>(
        &self,
        py: Python<'py>,
        program: Py<Prepared>,
        input: Vec<u8>,
    ) -> PyResult<Bound<'py, PyAny>> {
        let inner = self.inner.clone();

        pyo3_async_runtimes::tokio::future_into_py(py, async move {
            let program = &program.get().inner;

            let output = (inner.execute(program, &input).await)
                .map_err(|e| PyValueError::new_err(e.to_string()))?;

            Ok(output)
        })
    }

    fn get_interface<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let inner = self.inner.clone();

        pyo3_async_runtimes::tokio::future_into_py(py, async move {
            inner
                .get_interface()
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
