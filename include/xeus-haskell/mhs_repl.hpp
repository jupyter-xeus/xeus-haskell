#pragma once
#include <expected>
#include <string_view>
#include "Repl_stub.h"

namespace xeus_haskell {

class MicroHsRepl {
public:
    MicroHsRepl(); // calls mhs_init() and mhs_repl_new()
    ~MicroHsRepl(); // calls mhs_repl_free()

    std::expected<std::string, std::string> execute(std::string_view code);

private:
    uintptr_t context = 0;
    std::expected<std::string, std::string> define(std::string_view code);
    std::expected<std::string, std::string> run(std::string_view code);
};

} // namespace xeus_haskell

