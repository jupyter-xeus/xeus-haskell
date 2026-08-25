#pragma once

#include <string>
#include <string_view>
#include <vector>

#include "nlohmann/json.hpp"

namespace xeus_haskell::ghc_protocol
{
    struct output_chunk
    {
        bool rich;
        std::string mime_type;
        std::string content;
    };

    struct execution_response
    {
        bool ok = false;
        std::string ename;
        std::string evalue;
        std::vector<std::string> traceback;
        std::string stdout_text;
        std::string stderr_text;
        bool has_result = false;
        std::string result;
    };

    struct completion_response
    {
        bool ok = false;
        std::vector<std::string> matches;
        int cursor_start = 0;
        int cursor_end = 0;
    };

    struct inspection_response
    {
        bool found = false;
        nlohmann::json data = nlohmann::json::object();
    };

    struct completeness_response
    {
        bool ok = false;
        std::string status = "unknown";
        std::string indent = "  ";
    };

    std::vector<output_chunk> parse_display_protocol(std::string_view output);
    execution_response normalize_execution(const nlohmann::json& result);
    completion_response normalize_completion(const nlohmann::json& result,
                                               int cursor_pos);
    inspection_response normalize_inspection(const nlohmann::json& result);
    completeness_response normalize_completeness(const nlohmann::json& result);
}
