#pragma once
#include <string>
#include <string_view>
#include "Repl_stub.h"

namespace xeus_haskell {

struct repl_result
{
    bool ok;
    std::string output;
    std::string error;
    std::string mime_type = "text/plain";
};

class MicroHsRepl {
public:
    MicroHsRepl(); // calls mhs_init() and mhs_repl_new()
    ~MicroHsRepl(); // calls mhs_repl_free()

    repl_result execute(std::string_view code);

private:
    uintptr_t context = 0;
};

} // namespace xeus_haskell
