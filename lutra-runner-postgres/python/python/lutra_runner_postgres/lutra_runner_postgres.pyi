import typing
import lutra_bin

## A PostgreSQL runner. Requires programs in sql-pg format.
## Connects to PostgreSQL database.
class Runner:
    ## Creates a Runner and connects to PostgreSQL.
    ##
    ## Configuration can be parsed from libpq-style connection strings. These strings come in two formats:
    ## - Url or
    ## - Key-Value
    ## For exact format of the string, refer to [tokio_postgres::Config] documentation.
    @staticmethod
    async def connect(config: str) -> Runner: ...

    ## Run a program.
    ##
    ## This is helper function for [Run::prepare] followed by [Run::execute],
    ## with input encoding and output decoding.
    async def run[I, O](self, program: lutra_bin.Program[I, O], inp: I) -> O: ...

    ## Prepares a program for execution and returns a handle, which can be
    ## used with [Run::execute].
    ##
    ## If the program is invalid, error is returned either now or later by [Run::execute].
    ##
    ## When the handle is returned, the program might not be
    ## fully prepared yet, so first execution of the program
    ## might take longer then subsequent [Run::execute] calls.
    async def prepare(self, program: bytes) -> typing.Any: ...

    ## Execute a prepared program, encode input and decode output.
    ## Program's format must be sql-pg.
    async def execute(self, program: typing.Any, input: bytes) -> bytes: ...

    ## Return static interface of this runner as Lutra source code.
    ##
    ## For each table there will be a type definition and from&insert functions.
    async def get_interface(self) -> str: ...

    ## Releases any claimed resources or network connections.
    async def shutdown(self) -> None: ...

if typing.TYPE_CHECKING:
    from . import Run

    # statically assert that Runner implements Run protocol
    _: type[Run] = Runner
