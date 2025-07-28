# TODO: move module should be in an independent package, analogous to lutra_runner rust crate

import typing
import lutra_bin as lb


## Ability to execute a lutra program.
class Run(typing.Protocol):
    ## Run a program.
    ##
    ## This is helper function for [Run::prepare] followed by [Run::execute],
    ## with input encoding and output decoding.
    async def run[I, O](self, program: lb.Program[I, O], inp: I) -> O: ...

    ## Prepares a program for execution and returns a handle, which can be
    ## used with [Run::execute]. Does not block.
    ##
    ## If the program is invalid, error is returned either now or later by [Run::execute].
    ##
    ## When the handle is returned, the program might not be
    ## fully prepared yet, so first execution of the program
    ## might take longer then subsequent [Run::execute] calls.
    async def prepare(self, program: bytes) -> typing.Any: ...

    ## Execute a prepared program.
    ## Program's format must match the format supported by this runner.
    async def execute(self, program: typing.Any, input: bytes) -> bytes: ...

    ## Return static interface of this runner as Lutra source code.
    ##
    ## Runners can provide implementations for functions that are not part of
    ## standard library. This function returns definitions of these functions as
    ## Lutra source code.
    ##
    ## For example: interpreter can provide `fs::read_parquet()`
    ## and PostgreSQL runner can provide `sql::read_table()`.
    async def get_interface(self) -> str: ...

    ## Releases any claimed resources or network connections.
    async def shutdown(self) -> None: ...


async def run[I, O](runner: Run, program: lb.Program[I, O], inp: I) -> O:
    inp_bin = lb.BytesMut()
    r = program.input_codec.encode_head(inp, inp_bin)
    program.input_codec.encode_body(inp, r, inp_bin)

    prepared = await runner.prepare(program.program_bin)
    output_bin = await runner.execute(prepared, inp_bin.into_bytes())
    return program.output_codec.decode(output_bin)
