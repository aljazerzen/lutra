#pragma once

// Header-only C++20 implementation of the lutra-bin wire format.
// All sizes/positions below are BYTES (unlike the IR's bit-sized layouts).
// Heads are packed, without alignment. Bodies are appended after their heads.
#include <bit>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <span>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

namespace lutra_cpp {

/// Malformed wire data, invalid UTF-8, or an unrepresentable wire size.
struct error : std::runtime_error {
  using std::runtime_error::runtime_error;
};

inline std::size_t checked_add(std::size_t a, std::size_t b) {
  if (b > std::numeric_limits<std::size_t>::max() - a)
    throw error("Lutra size overflow");
  return a + b;
}
inline std::size_t checked_mul(std::size_t a, std::size_t b) {
  if (a != 0 && b > std::numeric_limits<std::size_t>::max() / a)
    throw error("Lutra size overflow");
  return a * b;
}
inline std::uint32_t checked_u32(std::size_t value) {
  if (value > std::numeric_limits<std::uint32_t>::max())
    throw error("Lutra size exceeds uint32");
  return static_cast<std::uint32_t>(value);
}

/// Owns the output. Allocation zero-fills padding; never retain pointers into
/// data across an allocation, since appending a body may reallocate it.
struct writer {
  std::vector<std::uint8_t> data;

  std::size_t allocate(std::size_t n) {
    const auto pos = data.size();
    const auto end = checked_add(pos, n);
    if (end > data.max_size())
      throw error("Lutra output is too large");
    data.resize(end, 0);
    return pos;
  }
  void check(std::size_t pos, std::size_t n) const {
    if (pos > data.size() || n > data.size() - pos)
      throw error("Lutra write outside allocated head");
  }
  /// Write a little-endian integer of 0..8 bytes (also useful for enum tags).
  void write_uint(std::size_t pos, std::uint64_t value, std::size_t n) {
    if (n > 8 || (n < 8 && (value >> (n * 8)) != 0))
      throw error("Lutra integer does not fit");
    check(pos, n);
    for (std::size_t i = 0; i < n; ++i)
      data[pos + i] = static_cast<std::uint8_t>(value >> (i * 8));
  }
  /// Offsets are unsigned u32, relative to the pointer field itself, NOT
  /// relative to the enclosing value or the start of the buffer.
  void write_offset(std::size_t pos, std::size_t target) {
    if (target < pos || target > data.size())
      throw error("Invalid Lutra offset");
    write_uint(pos, checked_u32(target - pos), 4);
  }
};

/// Non-owning bounds-checked view. The caller retains ownership of the bytes.
struct reader {
  std::span<const std::uint8_t> data;

  explicit reader(std::span<const std::uint8_t> bytes) : data(bytes) {}
  void check(std::size_t pos, std::size_t n) const {
    if (pos > data.size() || n > data.size() - pos)
      throw error("Truncated Lutra data");
  }
  std::uint64_t read_uint(std::size_t pos, std::size_t n) const {
    if (n > 8)
      throw error("Invalid Lutra integer width");
    check(pos, n);
    std::uint64_t value = 0;
    for (std::size_t i = 0; i < n; ++i)
      value |= std::uint64_t{data[pos + i]} << (i * 8);
    return value;
  }
  /// Resolve a relative u32 pointer, allowing a one-past-end empty body.
  std::size_t read_offset(std::size_t pos) const {
    const auto offset = static_cast<std::size_t>(read_uint(pos, 4));
    const auto target = checked_add(pos, offset);
    check(target, 0);
    return target;
  }
};

/// Generated tuple/enum codecs specialize this template. encode writes an
/// already allocated head at pos, appending any bodies with writer::allocate.
/// decode must validate its entire head, including any unused enum padding.
/// Both methods use absolute positions; head_size is a compile-time byte count.
template <class T> struct codec;

template <class T>
  requires (std::is_integral_v<T> && !std::is_same_v<T, bool>
            && (sizeof(T) == 1 || sizeof(T) == 2 || sizeof(T) == 4 || sizeof(T) == 8))
struct codec<T> {
  static constexpr std::size_t head_size = sizeof(T);
  static void encode(writer& w, std::size_t pos, const T& value) {
    using U = std::make_unsigned_t<T>;
    w.write_uint(pos, static_cast<U>(value), head_size);
  }
  static T decode(reader& r, std::size_t pos) {
    using U = std::make_unsigned_t<T>;
    const auto bits = static_cast<U>(r.read_uint(pos, head_size));
    return std::bit_cast<T>(bits);
  }
};

template <> struct codec<bool> {
  static constexpr std::size_t head_size = 1;
  static void encode(writer& w, std::size_t pos, const bool& value) {
    w.write_uint(pos, value ? 1 : 0, 1);
  }
  static bool decode(reader& r, std::size_t pos) {
    // Matches lutra-bin: every nonzero byte decodes as true.
    return r.read_uint(pos, 1) != 0;
  }
};

template <class T>
  requires (std::is_same_v<T, float> || std::is_same_v<T, double>)
struct codec<T> {
  static_assert(std::numeric_limits<T>::is_iec559);
  static constexpr std::size_t head_size = sizeof(T);
  using bits_type = std::conditional_t<sizeof(T) == 4, std::uint32_t, std::uint64_t>;
  static_assert(sizeof(T) == sizeof(bits_type));
  static void encode(writer& w, std::size_t pos, const T& value) {
    codec<bits_type>::encode(w, pos, std::bit_cast<bits_type>(value));
  }
  static T decode(reader& r, std::size_t pos) {
    return std::bit_cast<T>(codec<bits_type>::decode(r, pos));
  }
};

template <> struct codec<std::monostate> {
  static constexpr std::size_t head_size = 0;
  static void encode(writer& w, std::size_t pos, const std::monostate&) {
    w.check(pos, 0);
  }
  static std::monostate decode(reader& r, std::size_t pos) {
    r.check(pos, 0);
    return {};
  }
};

/// Reject truncated/overlong sequences, surrogates, and code points > U+10FFFF.
inline void validate_utf8(std::span<const std::uint8_t> bytes) {
  std::size_t i = 0;
  while (i < bytes.size()) {
    const auto first = bytes[i++];
    if (first < 0x80)
      continue;
    std::size_t extra;
    std::uint32_t cp;
    std::uint32_t minimum;
    if (first >= 0xc2 && first <= 0xdf) {
      extra = 1; cp = first & 0x1f; minimum = 0x80;
    } else if (first >= 0xe0 && first <= 0xef) {
      extra = 2; cp = first & 0x0f; minimum = 0x800;
    } else if (first >= 0xf0 && first <= 0xf4) {
      extra = 3; cp = first & 0x07; minimum = 0x10000;
    } else {
      throw error("Invalid UTF-8 in Lutra text");
    }
    if (extra > bytes.size() - i)
      throw error("Truncated UTF-8 in Lutra text");
    for (std::size_t j = 0; j < extra; ++j) {
      const auto next = bytes[i++];
      if ((next & 0xc0) != 0x80)
        throw error("Invalid UTF-8 continuation in Lutra text");
      cp = (cp << 6) | (next & 0x3f);
    }
    if (cp < minimum || cp > 0x10ffff || (cp >= 0xd800 && cp <= 0xdfff))
      throw error("Invalid UTF-8 code point in Lutra text");
  }
}

/// Text is an eight-byte head: relative u32 pointer, then u32 byte length.
template <> struct codec<std::string> {
  static constexpr std::size_t head_size = 8;
  static void encode(writer& w, std::size_t pos, const std::string& value) {
    w.check(pos, head_size);
    const auto length = checked_u32(value.size());
    const auto bytes = std::span<const std::uint8_t>(
      reinterpret_cast<const std::uint8_t*>(value.data()), value.size());
    validate_utf8(bytes);
    const auto body = w.allocate(value.size());
    w.write_offset(pos, body);
    w.write_uint(pos + 4, length, 4);
    for (std::size_t i = 0; i < bytes.size(); ++i)
      w.data[body + i] = bytes[i];
  }
  static std::string decode(reader& r, std::size_t pos) {
    r.check(pos, head_size);
    const auto body = r.read_offset(pos);
    const auto length = static_cast<std::size_t>(r.read_uint(pos + 4, 4));
    r.check(body, length);
    const auto bytes = r.data.subspan(body, length);
    validate_utf8(bytes);
    if (length == 0)
      return {};
    return std::string(reinterpret_cast<const char*>(bytes.data()), length);
  }
};

/// Arrays use the same head as text, but length counts elements. All element
/// heads are contiguous and reserved BEFORE any element bodies are appended.
template <class T, class Allocator> struct codec<std::vector<T, Allocator>> {
  static constexpr std::size_t head_size = 8;
  using vector_type = std::vector<T, Allocator>;
  static void encode(writer& w, std::size_t pos, const vector_type& value) {
    w.check(pos, head_size);
    const auto count = checked_u32(value.size());
    const auto body = w.allocate(checked_mul(value.size(), codec<T>::head_size));
    w.write_offset(pos, body);
    w.write_uint(pos + 4, count, 4);
    auto element = body;
    for (std::size_t i = 0; i < value.size(); ++i) {
      // vector<bool>'s proxy converts to the bool expected by its codec.
      codec<T>::encode(w, element, value[i]);
      element += codec<T>::head_size;
    }
  }
  static vector_type decode(reader& r, std::size_t pos) {
    r.check(pos, head_size);
    const auto body = r.read_offset(pos);
    const auto count = static_cast<std::size_t>(r.read_uint(pos + 4, 4));
    r.check(body, checked_mul(count, codec<T>::head_size));
    vector_type result;
    if (count > result.max_size())
      throw error("Lutra array is too large");
    result.reserve(count);
    auto element = body;
    for (std::size_t i = 0; i < count; ++i) {
      result.push_back(codec<T>::decode(r, element));
      element += codec<T>::head_size;
    }
    return result;
  }
};

/// Encode a root head and all its bodies. Allocation failures use standard
/// library exceptions; wire/validation failures throw lutra_cpp::error.
template <class T> std::vector<std::uint8_t> encode(const T& value) {
  writer w;
  w.allocate(codec<T>::head_size);
  codec<T>::encode(w, 0, value);
  return std::move(w.data);
}

/// Decode a root value. Trailing bytes and noncanonical body placement are
/// permitted, as in lutra-bin; every accessed head/body is bounds checked.
/// Untrusted inputs may still require application-level memory/depth limits.
template <class T> T decode(std::span<const std::uint8_t> bytes) {
  reader r(bytes);
  r.check(0, codec<T>::head_size);
  return codec<T>::decode(r, 0);
}

} // namespace lutra_cpp
