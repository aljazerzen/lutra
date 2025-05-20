# Import all definitions from the lutra_bin rust library into the root module
from .lutra_bin import *  # noqa: F403

__all__ = lutra_bin.__all__ + [  # noqa: F405
    # python
    "encode",
    "decode",
    "TypedProgram",
]


# Python part of the lutra_bin package
import typing

if typing.TYPE_CHECKING:

    class BytesMut:
        def __init__(self): ...
        def into_bytes(self) -> bytes: ...


class Codec[T](typing.Protocol):
    def head_bytes(self) -> int: ...
    def decode(self, buf: bytes) -> T: ...
    def encode_head(self, value: T, buf: BytesMut) -> typing.Any: ...
    def encode_body(self, value: T, head_residual: typing.Any, buf: BytesMut): ...


class Encodable(typing.Protocol):
    @classmethod
    def decode(cls, buf: bytes) -> typing.Self: ...
    def encode_head(self, buf: BytesMut) -> typing.Any: ...
    def encode_body(self, head_residual: typing.Any, buf: BytesMut): ...


class EncodableCodec[T: Encodable](Codec):
    def __init__(self, encodable_cls: type[T]):
        self.encodable_cls = encodable_cls

    def head_bytes(self) -> int:
        return self.encodable_cls.head_bytes()

    def decode(self, buf: bytes) -> T:
        return self.encodable_cls.decode(buf)

    def encode_head(self, value: T, buf: BytesMut) -> typing.Any:
        value.encode_head(buf)

    def encode_body(self, value: T, head_residual: typing.Any, buf: BytesMut):
        value.encode_body(head_residual, buf)


def encode[T: Encodable](obj: T) -> bytes:
    buf = BytesMut()
    residuals = obj.encode_head(buf)
    obj.encode_body(residuals, buf)
    return buf.take()


def decode[T: Encodable](typ: type[T], buf: bytes) -> T:
    return typ.decode(buf)


class TypedProgram[I: Encodable, O: Encodable]:
    __slots__ = ("program", "input_ty", "output_ty")

    def __init__(
        self,
        program: bytes,
        input_ty: typing.Type[I],
        output_ty: typing.Type[O],
    ):
        self.program = sr.Program.decode(program)  # noqa: F405
        self.input_ty = input_ty
        self.output_ty = output_ty
