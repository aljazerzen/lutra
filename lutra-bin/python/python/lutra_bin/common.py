import typing
from . import BytesMut


class Codec[T](typing.Protocol):
    def head_bytes(self) -> int: ...
    def decode(self, buf: bytes) -> T: ...
    def encode_head(self, value: T, buf: BytesMut) -> typing.Any: ...
    def encode_body(self, value: T, head_residual: typing.Any, buf: BytesMut): ...


class Encodable(typing.Protocol):
    @classmethod
    def codec(cls) -> Codec[typing.Self]: ...


def encode[T: Encodable](obj: T) -> bytes:
    codec = obj.codec()

    buf = BytesMut()
    residuals = codec.encode_head(obj, buf)
    codec.encode_body(obj, residuals, buf)
    return buf.into_bytes()


def decode[T: Encodable](typ: type[T], buf: bytes) -> T:
    codec = typ.codec()
    return codec.decode(buf)


class Program[I, O]:
    __slots__ = ("program_bin", "input_codec", "output_codec")

    def __init__(
        self,
        program_bin: bytes,
        input_codec: Codec[I],
        output_codec: Codec[O],
    ):
        self.program_bin = program_bin
        self.input_codec = input_codec
        self.output_codec = output_codec
