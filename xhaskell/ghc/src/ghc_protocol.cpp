#include "ghc_protocol.hpp"

#include <utility>

namespace xeus_haskell::ghc_protocol
{
    namespace
    {
        bool supported_mime_type(std::string_view mime_type)
        {
            return mime_type == "text/plain"
                || mime_type == "text/html"
                || mime_type == "text/latex"
                || mime_type == "text/markdown";
        }

        void append_plain(std::vector<output_chunk>& chunks, std::string text)
        {
            if (text.empty())
            {
                return;
            }
            if (!chunks.empty() && !chunks.back().rich)
            {
                chunks.back().content += text;
            }
            else
            {
                chunks.push_back({false, "text/plain", std::move(text)});
            }
        }

        std::vector<std::string> traceback_from(const nlohmann::json& result,
                                                const std::string& fallback)
        {
            if (result.contains("traceback") && result["traceback"].is_array())
            {
                auto traceback = result["traceback"].get<std::vector<std::string>>();
                if (!traceback.empty())
                {
                    return traceback;
                }
            }
            return {fallback};
        }
    }

    std::vector<output_chunk> parse_display_protocol(std::string_view output)
    {
        constexpr char stx = '\x02';
        constexpr char unit_separator = '\x1f';
        constexpr char etx = '\x03';

        std::vector<output_chunk> chunks;
        std::size_t cursor = 0;
        while (cursor < output.size())
        {
            const std::size_t start = output.find(stx, cursor);
            if (start == std::string_view::npos)
            {
                append_plain(chunks, std::string(output.substr(cursor)));
                break;
            }
            append_plain(chunks, std::string(output.substr(cursor, start - cursor)));

            const std::size_t separator = output.find(unit_separator, start + 1);
            const std::size_t end = separator == std::string_view::npos
                ? std::string_view::npos
                : output.find(etx, separator + 1);
            if (separator == std::string_view::npos || end == std::string_view::npos)
            {
                append_plain(chunks, std::string(output.substr(start)));
                break;
            }

            const std::string_view mime_type =
                output.substr(start + 1, separator - start - 1);
            if (supported_mime_type(mime_type))
            {
                chunks.push_back({
                    true,
                    std::string(mime_type),
                    std::string(output.substr(separator + 1, end - separator - 1))
                });
            }
            else
            {
                append_plain(chunks, std::string(output.substr(start, end - start + 1)));
            }

            cursor = end + 1;
            if (cursor < output.size() && output[cursor] == '\r')
            {
                ++cursor;
            }
            if (cursor < output.size() && output[cursor] == '\n')
            {
                ++cursor;
            }
        }
        return chunks;
    }

    execution_response normalize_execution(const nlohmann::json& result)
    {
        execution_response response;
        if (result.is_discarded() || !result.value("ok", false))
        {
            response.ename = result.is_discarded()
                ? "RuntimeError" : result.value("ename", "GHCError");
            response.evalue = result.is_discarded()
                ? "Invalid response from the GHC runtime"
                : result.value("evalue", "GHC execution failed");
            response.traceback = result.is_discarded()
                ? std::vector<std::string>{response.evalue}
                : traceback_from(result, response.evalue);
            return response;
        }

        response.ok = true;
        response.stdout_text = result.value("stdout", "");
        response.stderr_text = result.value("stderr", "");
        response.has_result = result.value("has_result", false);
        response.result = result.value("result", "");
        return response;
    }

    completion_response normalize_completion(const nlohmann::json& result,
                                               int cursor_pos)
    {
        completion_response response;
        response.cursor_start = cursor_pos;
        response.cursor_end = cursor_pos;
        if (result.is_discarded() || !result.value("ok", false))
        {
            return response;
        }
        response.ok = true;
        response.matches = result.value("matches", std::vector<std::string>{});
        response.cursor_start = result.value("cursor_start", cursor_pos);
        response.cursor_end = result.value("cursor_end", cursor_pos);
        return response;
    }

    inspection_response normalize_inspection(const nlohmann::json& result)
    {
        inspection_response response;
        if (result.is_discarded()
            || !result.value("ok", false)
            || !result.value("found", false))
        {
            return response;
        }
        response.found = true;
        response.data = result.value("data", nlohmann::json::object());
        return response;
    }

    completeness_response normalize_completeness(const nlohmann::json& result)
    {
        completeness_response response;
        if (result.is_discarded() || !result.value("ok", false))
        {
            return response;
        }
        response.ok = true;
        response.status = result.value("status", "unknown");
        response.indent = result.value("indent", "  ");
        return response;
    }
}
