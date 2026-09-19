#include "generated.hpp"
#include <cassert>
#include <iostream>

int main() {
  namespace g = lutra_generated;
  const g::Movie movie{54, "Hello", g::Genre{g::Genre::other{"documentary"}}};
  const auto bytes = lutra_cpp::encode(movie);
  const auto decoded = lutra_cpp::decode<g::Movie>(bytes);
  assert(decoded.id == movie.id);
  assert(decoded.title == movie.title);
  assert(std::get<g::Genre::other>(decoded.genre.value).value == "documentary");
  std::cout << decoded.title << " (" << bytes.size() << " bytes)\n";
}
