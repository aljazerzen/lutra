# C++ bindings

Requires the Lutra CLI and a C++20 compiler (GCC or Clang with a C++20 standard library).
No additional C++ library or linker dependency is needed.

From this directory:

```sh
lutra gen-code --project ./types.lt ./generated.hpp
c++ -std=c++20 -Wall -Wextra -pedantic main.cpp -o example
./example
```

`generated.hpp` contains native types and the header-only Lutra binary codec.
See [C++ usage](../../website/docs/usage/cpp.md) for options and limitations.
