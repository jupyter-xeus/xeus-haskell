#include <boost/ut.hpp>
#include <xeus-haskell/mhs_repl.hpp>

#include <algorithm>
#include <iostream>
#include <string_view>

#include "../common/test_helpers.hpp"

using namespace boost::ut;
using namespace std::string_literals;

auto repl_instance = []() -> xeus_haskell::MicroHsRepl& {
    static xeus_haskell::MicroHsRepl repl;
    return repl;
};

int main() {
    "positive repl test"_test = [] {
        auto& repl = repl_instance();
        auto result = repl.execute("1 + 1");

        expect(result.ok);
        test_helpers::expect_trim_eq(result.output, "2");
    };

    "MicroHs runtime is built without GMP"_test = [] {
        auto& repl = repl_instance();
        auto declaration = repl.execute(
            "foreign import capi \"want_gmp\" xhWantGmp :: Int");
        auto result = repl.execute("xhWantGmp");
        expect(declaration.ok) << declaration.error;
        expect(result.ok) << result.error;
        test_helpers::expect_trim_eq(result.output, "0");
    };

    "stdout is captured"_test = [] {
        auto& repl = repl_instance();
        auto res = repl.execute("putStrLn \"hello from repl\"");
        expect(res.ok);
        expect(that % res.output.find("hello from repl") != std::string::npos);
    };

    "leading comment expressions execute"_test = [] {
        auto& repl = repl_instance();
        auto res = repl.execute("-- Hello World\nprint \"Hello World!\"");
        expect(res.ok) << res.error;
        expect(that % res.output.find("Hello World!") != std::string::npos);
    };

    "leading block comment expressions execute"_test = [] {
        auto& repl = repl_instance();
        const auto code = R"({- block
comment -}
print "Hello World!")";
        auto res = repl.execute(code);
        expect(res.ok) << res.error;
        expect(that % res.output.find("Hello World!") != std::string::npos);
    };

    "comments with blank lines before expression execute"_test = [] {
        auto& repl = repl_instance();
        const auto code = R"(-- first

-- second

print "Hello World!")";
        auto res = repl.execute(code);
        expect(res.ok) << res.error;
        expect(that % res.output.find("Hello World!") != std::string::npos);
    };

    "definitions persist"_test = [] {
        auto& repl = repl_instance();
        auto def_result = repl.execute("xh_def_test = 40 + 2");
        expect(def_result.ok);

        auto res = repl.execute("xh_def_test");
        expect(res.ok);
        test_helpers::expect_trim_eq(res.output, "42");
    };

    "redefinitions replace old values"_test = [] {
        auto& repl = repl_instance();
        auto first = repl.execute("xh_redef_test = 1");
        expect(first.ok);

        auto redef = repl.execute("xh_redef_test = 5");
        expect(redef.ok);

        auto res = repl.execute("xh_redef_test");
        expect(res.ok);
        test_helpers::expect_trim_eq(res.output, "5");
    };

    "completion candidates include definitions and reserved ids"_test = [] {
        auto& repl = repl_instance();
        auto def_res = repl.execute("xh_completion_test_value = 7");
        expect(def_res.ok) << def_res.error;

        const auto candidates = repl.completion_candidates();
        auto has = [&](std::string_view needle) {
            return std::ranges::find(candidates, needle) != candidates.end();
        };

        expect(has("xh_completion_test_value"));
        expect(has("where")); // reserved keyword
    };

    "derived Typeable definitions stay usable"_test = [] {
        auto& repl = repl_instance();
        const auto type_def = R"(
data XhTypeableRecord = XhTypeableRecord
  { xhField :: Int
  } deriving (Show, Typeable)
)";
        auto def_res = repl.execute(type_def);
        expect(def_res.ok) << def_res.error;

        // Regression test for B@._mkTyCon lookup: evaluating after the definition
        // must continue to work.
        auto eval_res = repl.execute("xhField (XhTypeableRecord 42)");
        expect(eval_res.ok) << eval_res.error;
        test_helpers::expect_trim_eq(eval_res.output, "42");

        auto simple_res = repl.execute("20 + 22");
        expect(simple_res.ok) << simple_res.error;
        test_helpers::expect_trim_eq(simple_res.output, "42");
    };

    "expressions evaluate"_test = [] {
        auto& repl = repl_instance();
        auto res = repl.execute("let (a, b) = (10, 20) in a + b");
        expect(res.ok);
        test_helpers::expect_trim_eq(res.output, "30");
    };

    "type command reports expression type"_test = [] {
        auto& repl = repl_instance();
        auto res = repl.execute(":type \"Hello World\"");
        expect(res.ok) << res.error;
        expect(that % res.output.find("\"Hello World\" ::") != std::string::npos);
        const bool is_char_list =
            res.output.find("[Char]") != std::string::npos ||
            res.output.find("String") != std::string::npos;
        expect(is_char_list);
    };

    "kind command reports type kind"_test = [] {
        auto& repl = repl_instance();
        auto res = repl.execute(":kind Int");
        expect(res.ok) << res.error;
        expect(that % res.output.find("Int ::") != std::string::npos);
        const bool has_kind =
            res.output.find("*") != std::string::npos ||
            res.output.find("Type") != std::string::npos;
        expect(has_kind);
    };

    "expression errors are reported"_test = [] {
        auto& repl = repl_instance();
        auto res = repl.execute("1 + \"1\"");
        expect(!res.ok);
        expect(!res.error.empty());
    };

    "inspect reports user definitions"_test = [] {
        auto& repl = repl_instance();
        auto def = repl.execute("xh_inspect_contract = 42");
        expect(def.ok) << def.error;
        const auto info = repl.inspect("xh_inspect_contract");
        expect(that % info.find("xh_inspect_contract ::") != std::string::npos);
    };

    "completeness distinguishes complete and incomplete input"_test = [] {
        auto& repl = repl_instance();
        expect(repl.is_complete("1 + 1") == "complete");
        expect(repl.is_complete("[1, 2, 3") == "incomplete");
    };

    "html latex and markdown frames retain their mime type"_test = [] {
        auto& repl = repl_instance();
        const auto check = [&](std::string_view mime_type,
                               std::string_view content) {
            const std::string code = "putStr \"\\x02" + std::string(mime_type)
                + "\\x1F" + std::string(content) + "\\x03\"";
            auto result = repl.execute(code);
            expect(result.ok) << result.error;
            expect(result.mime_type == mime_type);
            test_helpers::expect_trim_eq(result.output, content);
        };
        check("text/html", "<strong>contract</strong>");
        check("text/latex", "$x^2$");
        check("text/markdown", "**contract**");
    };
}
