#pragma once

#include <boost/ut.hpp>

#include <cctype>
#include <ranges>
#include <string>
#include <string_view>

namespace test_helpers {

inline auto trim(std::string_view s) -> std::string {
    auto is_space = [](unsigned char c) { return std::isspace(c); };

    auto trimmed = s
        | std::views::drop_while(is_space)
        | std::views::reverse
        | std::views::drop_while(is_space)
        | std::views::reverse;

    return {trimmed.begin(), trimmed.end()};
}

inline void expect_trim_eq(std::string_view a, std::string_view b) {
    boost::ut::expect(boost::ut::eq(trim(a), trim(b)))
        << "expected (ignoring spaces): " << a << " == " << b;
}

} // namespace test_helpers
