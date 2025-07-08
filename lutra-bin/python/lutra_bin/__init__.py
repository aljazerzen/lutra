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
    def codec(cls) -> Codec[typing.Self]: ...


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
    codec = obj.codec()

    buf = BytesMut()
    residuals = codec.encode_head(obj, buf)
    codec.encode_body(obj, residuals, buf)
    return buf.into_bytes()


def decode[T: Encodable](typ: type[T], buf: bytes) -> T:
    codec = typ.codec()
    return codec.decode(buf)


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
