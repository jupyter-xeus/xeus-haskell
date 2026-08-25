#include <boost/ut.hpp>

#include <string>

#include "ghc_protocol.hpp"

using namespace boost::ut;
using namespace xeus_haskell::ghc_protocol;

int main()
{
    "execution success preserves streams and expression result"_test = [] {
        const auto response = normalize_execution({
            {"ok", true},
            {"stdout", "hello\n"},
            {"stderr", "warning\n"},
            {"has_result", true},
            {"result", "42"},
        });
        expect(response.ok);
        expect(response.stdout_text == "hello\n");
        expect(response.stderr_text == "warning\n");
        expect(response.has_result);
        expect(response.result == "42");
    };

    "execution errors retain compiler diagnostics"_test = [] {
        const auto response = normalize_execution({
            {"ok", false},
            {"ename", "GHCError"},
            {"evalue", "Not in scope"},
            {"traceback", {"line one", "line two"}},
        });
        expect(!response.ok);
        expect(response.ename == "GHCError");
        expect(response.evalue == "Not in scope");
        expect(response.traceback.size() == 2_u);
    };

    "display protocol separates plain and rich output"_test = [] {
        const std::string output =
            "before\x02text/html\x1f<strong>html</strong>\x03\n"
            "middle\x02text/latex\x1f$x^2$\x03\n"
            "after\x02text/markdown\x1f**markdown**\x03";
        const auto chunks = parse_display_protocol(output);
        expect(chunks.size() == 6_u);
        expect(!chunks[0].rich && chunks[0].content == "before");
        expect(chunks[1].rich && chunks[1].mime_type == "text/html");
        expect(chunks[3].mime_type == "text/latex");
        expect(chunks[5].mime_type == "text/markdown");
    };

    "malformed and unsupported display frames remain plain text"_test = [] {
        const auto malformed = parse_display_protocol(
            "prefix\x02text/html\x1funterminated");
        expect(malformed.size() == 1_u);
        expect(!malformed[0].rich);
        expect(malformed[0].content.find("unterminated") != std::string::npos);

        const auto unsupported = parse_display_protocol(
            "\x02application/json\x1f{}\x03");
        expect(unsupported.size() == 1_u);
        expect(!unsupported[0].rich);
    };

    "completion preserves matches and cursor range"_test = [] {
        const auto response = normalize_completion({
            {"ok", true},
            {"matches", {"sharedAnswer"}},
            {"cursor_start", 0},
            {"cursor_end", 9},
        }, 9);
        expect(response.ok);
        expect(response.matches.size() == 1_u);
        expect(response.matches[0] == "sharedAnswer");
        expect(response.cursor_start == 0_i);
        expect(response.cursor_end == 9_i);
    };

    "inspection and completeness normalize protocol replies"_test = [] {
        const auto inspection = normalize_inspection({
            {"ok", true},
            {"found", true},
            {"data", {{"text/plain", "sharedAnswer :: Integer"}}},
        });
        expect(inspection.found);
        expect(inspection.data["text/plain"] == "sharedAnswer :: Integer");

        const auto completeness = normalize_completeness({
            {"ok", true}, {"status", "incomplete"}, {"indent", "  "},
        });
        expect(completeness.ok);
        expect(completeness.status == "incomplete");
    };
}
