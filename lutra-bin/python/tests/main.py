import lutra_bin
import unittest

# generated
from . import types_generated as types


class TestCase(unittest.TestCase):
    def test_x(self):
        v = types.x(42, "Hello world!", [True, False])

        b = lutra_bin.encode(v)
        self.assertEqual(
            b,
            b"*\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x0c\x00\x00\x00\x14\x00\x00\x00\x02\x00\x00\x00Hello\x20world!\x01\x00",
        )

        v2 = lutra_bin.decode(types.x, b)
        self.assertEqual(v, v2)

    def test_y(self):
        v = types.y([12, 55, 2])

        b = lutra_bin.encode(v)
        self.assertEqual(
            b,
            b"\x08\x00\x00\x00\x03\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x007\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00",
        )

        v2 = lutra_bin.decode(types.y, b)
        self.assertEqual(v, v2)

    def test_z(self):
        v = types.z(True)

        b = lutra_bin.encode(v)
        self.assertEqual(b, b"\x01")

        v2 = lutra_bin.decode(types.z, b)
        self.assertEqual(v, v2)

    @unittest.skip("todo")
    def test_u_01(self):
        v = types.u(f=True)

        b = lutra_bin.encode(v)
        self.assertEqual(b, b"\x00\x04\x00\x00\x00\x01")

        v2 = lutra_bin.decode(types.u, b)
        self.assertEqual(v, v2)

    @unittest.skip("todo")
    def test_u_02(self):
        v = types.u(g=True)

        b = lutra_bin.encode(v)
        self.assertEqual(b, b"\x01\x00\x00\x00\x00")

        v2 = lutra_bin.decode(types.u, b)
        self.assertEqual(v, v2)

    @unittest.skip("todo")
    def test_u_03(self):
        v = types.u(h=types.uh(-12, 3.16))

        b = lutra_bin.encode(v)
        self.assertEqual(
            b,
            b"\x02\x04\x00\x00\x00\xf4\xff\xff\xff\xff\xff\xff\xffH\xe1z\x14\xaeG\x09@",
        )

        v2 = lutra_bin.decode(types.u, b)
        self.assertEqual(v, v2)

    @unittest.skip("todo")
    def test_v(self):
        v = types.v(no=True)

        b = lutra_bin.encode(v)
        self.assertEqual(b, b"\x01")

        v2 = lutra_bin.decode(types.v, b)
        self.assertEqual(v, v2)

    @unittest.skip("todo")
    def test_t_01(self):
        v = types.t(single=2)

        b = lutra_bin.encode(v)
        self.assertEqual(b, b"\x00\x02\x00")

        v2 = lutra_bin.decode(types.t, b)
        self.assertEqual(v, v2)

    @unittest.skip("todo")
    def test_t_02(self):
        v = types.t(double=2342)

        b = lutra_bin.encode(v)
        self.assertEqual(b, b"\x01&\x09")

        v2 = lutra_bin.decode(types.t, b)
        self.assertEqual(v, v2)

    def test_p(self):
        v = types.p([2, 4], [5, 6, 7])

        b = lutra_bin.encode(v)
        self.assertEqual(
            b,
            b"\x10\x00\x00\x00\x02\x00\x00\x00\x18\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00",
        )

        v2 = lutra_bin.decode(types.p, b)
        self.assertEqual(v, v2)

    @unittest.skip("todo")
    def test_Tree(self):
        v = types.Tree(
            node=types.TreeNode(
                left=types.Tree(leaf=0),
                right=types.Tree(
                    node=types.TreeNode(
                        left=types.Tree(left=7),
                        right=types.Tree(left=10),
                    )
                ),
            )
        )

        b = lutra_bin.encode(v)
        self.assertEqual(
            b,
            b"\x01\x04\x00\x00\x00\x00\x09\x00\x00\x00\x01\x0c\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x0c\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00",
        )

        v2 = lutra_bin.decode(types.Tree, b)
        self.assertEqual(v, v2)

    @unittest.skip("todo")
    def test_opt_01(self):
        v = types.opt(none=True)

        b = lutra_bin.encode(v)
        self.assertEqual(b, b"\x00\x00\x00\x00\x00")

        v2 = lutra_bin.decode(types.opt, b)
        self.assertEqual(v, v2)

    @unittest.skip("todo")
    def test_opt_02(self):
        v = types.opt(some="text")

        b = lutra_bin.encode(v)
        self.assertEqual(b, b"\x01\x04\x00\x00\x00\x08\x00\x00\x00\x04\x00\x00\x00text")

        v2 = lutra_bin.decode(types.opt, b)
        self.assertEqual(v, v2)

    @unittest.skip("todo")
    def test_opt2_01(self):
        v = types.opt2(none=True)

        b = lutra_bin.encode(v)
        self.assertEqual(b, b"\x00\x00\x00")

        v2 = lutra_bin.decode(types.opt2, b)
        self.assertEqual(v, v2)

    @unittest.skip("todo")
    def test_opt2_02(self):
        v = types.opt2(some=65)

        b = lutra_bin.encode(v)
        self.assertEqual(b, b"\x01A\x00")

        v2 = lutra_bin.decode(types.opt2, b)
        self.assertEqual(v, v2)
