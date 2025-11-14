#include <boost/ut.hpp>
#include <xeus-haskell/mhs_repl.hpp>

#include <algorithm>
#include <chrono>
#include <cctype>
#include <iostream>
#include <ranges>
#include <string>
#include <string_view>

using namespace boost::ut;
using namespace std::string_literals;

auto trim(std::string_view s) -> std::string {
    auto is_space = [](unsigned char c) { return std::isspace(c); };

    auto trimmed = s 
        | std::views::drop_while(is_space)
        | std::views::reverse
        | std::views::drop_while(is_space)
        | std::views::reverse;

    return {trimmed.begin(), trimmed.end()};
}

auto expect_trim_eq = [](std::string_view a, std::string_view b) {
    expect(eq(trim(a), trim(b))) << "expected (ignoring spaces): " << a << " == " << b;
};

auto announce = [](std::string_view name) {
    log << "[test] " << name << "\n";
};

auto repl_instance = []() -> xeus_haskell::MicroHsRepl& {
    static xeus_haskell::MicroHsRepl repl;
    return repl;
};

int main() {
    "positive repl test"_test = [] {
        announce("positive repl test");
        auto& repl = repl_instance();
        auto result = repl.execute("1 + 1");

        expect_trim_eq(result.value(), "2");
    };

    "stdout is captured"_test = [] {
        announce("stdout is captured");
        auto& repl = repl_instance();
        auto res = repl.execute("putStrLn \"hello from repl\"");
        expect(res.has_value());
        expect(that % res.value().find("hello from repl") != std::string::npos);
    };

    "definitions persist"_test = [] {
        announce("definitions persist");
        auto& repl = repl_instance();
        auto def_result = repl.execute("xh_def_test = 40 + 2");
        expect(def_result.has_value());

        auto res = repl.execute("xh_def_test");
        expect(res.has_value());
        expect_trim_eq(res.value(), "42");
    };

    "expressions evaluate"_test = [] {
        announce("expressions evaluate");
        auto& repl = repl_instance();
        auto res = repl.execute("let (a, b) = (10, 20) in a + b");
        expect(res.has_value());
        expect_trim_eq(res.value(), "30");
    };

    "measure warm-up timing"_test = [] {
        announce("measure warm-up timing");
        auto& repl = repl_instance();
        auto measure = [&](std::string_view code) {
            const auto start = std::chrono::steady_clock::now();
            auto res = repl.execute(code);
            const auto end = std::chrono::steady_clock::now();
            const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();
            return std::pair{std::move(res), ms};
        };

        auto [first, first_ms] = measure("21 + 21");
        auto [second, second_ms] = measure("22 + 22");

        expect(first.has_value());
        expect(second.has_value());

        std::cout << "[timing] MicroHsRepl execute timings (ms): first="
                  << first_ms << " second=" << second_ms << std::endl;
    };

    "basic test"_test = [] {
        announce("basic test");
        expect(42_i == 42);
        expect(1_i != 2);
    };

    "sum test"_test = [] {
        announce("sum test");
        constexpr auto sum = [](auto... values) { return (values + ...); };
        expect(sum(1, 2) == 3_i);
        expect(sum(0) == 0_i);
    };
}
