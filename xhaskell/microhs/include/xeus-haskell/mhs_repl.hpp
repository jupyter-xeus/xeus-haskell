#pragma once
#include "Repl_stub.h"
#include <string>
#include <string_view>
#include <vector>

namespace xeus_haskell {

struct repl_result {
  bool ok;
  std::string output;
  std::string error;
  std::string mime_type = "text/plain";
};

class MicroHsRepl {
public:
  MicroHsRepl();  // calls mhs_init() and mhs_repl_new()
  ~MicroHsRepl(); // calls mhs_repl_free()

  repl_result execute(std::string_view code);
  std::vector<std::string> completion_candidates();
  std::string inspect(std::string_view name);
  std::string is_complete(std::string_view code);

private:
  uintptr_t context = 0;
};

} // namespace xeus_haskell
