#include <cpp11.hpp>
using namespace cpp11;

[[cpp11::register]] cpp11::strings byte_trunc_(cpp11::strings string,
                                               long long unsigned int size) {
  int n = string.size();
  writable::strings out(n);

  for (int i = 0; i < n; i++) {
    std::string current = string[i];

    if (size >= current.size()) {
      out[i] = current;
      continue;
    }

    int lower_bound = size - 3;

    signed char buffer[current.size()];
    memcpy(buffer, current.data(), current.size());

    for (int boundary = size; boundary >= lower_bound; boundary--) {
      // test if is valid utf8 char boundary
      if (static_cast<int>(buffer[boundary]) >= -0x40) {
        out[i] = current.substr(0, boundary);
        break;
      }
    }
  }

  return out;
}
