#pragma once

#include <functional>
#include <string>

namespace xeus_haskell {

std::string capture_stdout(const std::function<void()>& fn);

} // namespace xeus_haskell
