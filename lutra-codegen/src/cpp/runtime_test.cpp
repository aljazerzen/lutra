// Standalone test: c++ -std=c++20 -Wall -Wextra -pedantic runtime_test.cpp -o runtime_test
#include "runtime.hpp"
#include <cassert>
#include <cmath>
#include <functional>

using namespace lutra_cpp;

template <class T> void roundtrip(const T& value) {
  assert(decode<T>(encode(value)) == value);
}
void rejects(const std::function<void()>& f) {
  bool failed = false;
  try { f(); } catch (const error&) { failed = true; }
  assert(failed);
}

int main() {
  roundtrip(std::numeric_limits<std::int64_t>::min());
  roundtrip(std::numeric_limits<std::uint64_t>::max());
  roundtrip(std::int8_t{-1});
  roundtrip(std::int16_t{-321});
  roundtrip(std::int32_t{-987654});
  roundtrip(std::uint16_t{65535});
  roundtrip(std::uint32_t{987654});
  roundtrip(true);
  roundtrip(false);
  roundtrip(1.25f);
  roundtrip(-1.125);
  assert(std::signbit(decode<double>(encode(-0.0))));
  const auto nan = std::bit_cast<double>(std::uint64_t{0x7ff8000000000123});
  assert(std::bit_cast<std::uint64_t>(decode<double>(encode(nan))) == 0x7ff8000000000123);
  roundtrip(std::monostate{});
  roundtrip(std::string{});
  roundtrip(std::string("a\0b", 3));
  roundtrip(std::string("\xc2\xa2\xe2\x82\xac\xf0\x90\x8d\x88"));
  roundtrip(std::vector<bool>{false, true, false});
  roundtrip(std::vector<std::monostate>(3));
  roundtrip(std::vector<std::vector<std::string>>{{"a", "b"}, {}, {"xyz"}});
  assert((encode(std::uint32_t{0x12345678}) == std::vector<std::uint8_t>{0x78,0x56,0x34,0x12}));
  const std::vector<std::uint8_t> strings{
    8,0,0,0, 2,0,0,0, // array head
    16,0,0,0, 1,0,0,0, // first string pointer is relative to byte 8
    9,0,0,0, 2,0,0,0, // second string pointer is relative to byte 16
    'a','b','c'};
  assert((encode(std::vector<std::string>{"a", "bc"}) == strings));
  assert((decode<std::vector<std::string>>(strings) == std::vector<std::string>{"a", "bc"}));
  for (std::size_t n = 0; n < strings.size(); ++n)
    rejects([&] { (void)decode<std::vector<std::string>>(std::span(strings).first(n)); });
  for (const auto& invalid : {std::string("\x80"), std::string("\xc0\x80"),
       std::string("\xe0\x80\x80"), std::string("\xed\xa0\x80"),
       std::string("\xf4\x90\x80\x80"), std::string("\xf0\x90"),
       std::string("\xc2\x41")}) {
    rejects([&] { (void)encode(invalid); });
    auto wire = encode(std::string(invalid.size(), 'x'));
    for (std::size_t i = 0; i < invalid.size(); ++i)
      wire[8 + i] = static_cast<std::uint8_t>(invalid[i]);
    rejects([&] { (void)decode<std::string>(wire); });
  }
  rejects([] { (void)checked_add(std::numeric_limits<std::size_t>::max(), 1); });
  rejects([] { (void)checked_mul(std::numeric_limits<std::size_t>::max(), 2); });
  rejects([] {
    auto wire = encode(std::string{});
    wire[0] = 255;
    (void)decode<std::string>(wire);
  });
  rejects([] { writer w; w.allocate(4); w.write_offset(1, 0); });
  rejects([] { writer w; w.allocate(1); w.write_uint(0, 256, 1); });
  rejects([] { writer w; w.allocate(1); w.write_uint(0, 1, 0); });
  assert(decode<bool>(std::vector<std::uint8_t>{255}));
}
