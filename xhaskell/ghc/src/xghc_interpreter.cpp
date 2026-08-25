/***************************************************************************
 * Copyright (c) 2025, Masaya Taniguchi
 * Distributed under the terms of the Apache Software License 2.0.
 ****************************************************************************/

#include <string>
#include <string_view>
#include <vector>

#include <cstdlib>
#include <emscripten.h>

#include "nlohmann/json.hpp"
#include "xeus/xhelper.hpp"

#include "xeus-haskell/xeus_haskell_config.hpp"
#include "ghc_protocol.hpp"
#include "xghc_interpreter.hpp"

namespace nl = nlohmann;

namespace xeus_haskell
{
    EM_ASYNC_JS(char*, await_ghci_request,
                (const char* operation, const char* code, int cursor, int detail), {
        const result = await Module['xhaskellGhcRequest'](
            UTF8ToString(operation), UTF8ToString(code), cursor, detail);
        const bytes = new TextEncoder().encode(result);
        const response = _malloc(bytes.length + 1);
        HEAPU8.set(bytes, response);
        HEAPU8[response + bytes.length] = 0;
        return response;
    });

    namespace
    {
        nl::json request_ghci(const std::string& operation,
                              const std::string& code,
                              int cursor = 0,
                              int detail = 0)
        {
            char* response = await_ghci_request(
                operation.c_str(), code.c_str(), cursor, detail);
            const nl::json result = nl::json::parse(response, nullptr, false);
            std::free(response);
            return result;
        }

        void publish_protocol_output(ghc_interpreter& interpreter,
                                     const std::string& output,
                                     bool expression_result,
                                     int execution_counter)
        {
            for (auto& chunk : ghc_protocol::parse_display_protocol(output))
            {
                if (chunk.rich && chunk.mime_type != "text/plain")
                {
                    nl::json data;
                    data[chunk.mime_type] = std::move(chunk.content);
                    interpreter.display_data(
                        std::move(data),
                        nl::json::object(),
                        nl::json::object()
                    );
                }
                else if (expression_result)
                {
                    nl::json data;
                    data["text/plain"] = std::move(chunk.content);
                    interpreter.publish_execution_result(
                        execution_counter,
                        std::move(data),
                        nl::json::object()
                    );
                }
                else
                {
                    interpreter.publish_stream("stdout", chunk.content);
                }
            }
        }
    }

    ghc_interpreter::ghc_interpreter()
    {
        xeus::register_interpreter(this);
    }

    void ghc_interpreter::configure_impl()
    {
    }

    void ghc_interpreter::execute_request_impl(send_reply_callback cb,
                                                int execution_counter,
                                                const std::string& code,
                                                xeus::execute_request_config config,
                                                nl::json /*user_expressions*/)
    {
        const auto result = ghc_protocol::normalize_execution(
            request_ghci("execute", code));
        if (!result.ok)
        {
            publish_execution_error(result.ename, result.evalue, result.traceback);
            cb(xeus::create_error_reply(
                result.ename, result.evalue, result.traceback));
            return;
        }

        if (!config.silent)
        {
            if (!result.stdout_text.empty())
            {
                publish_protocol_output(
                    *this, result.stdout_text, false, execution_counter);
            }
            if (!result.stderr_text.empty())
            {
                publish_stream("stderr", result.stderr_text);
            }
            if (result.has_result)
            {
                publish_protocol_output(
                    *this, result.result, true, execution_counter
                );
            }
        }
        cb(xeus::create_successful_reply(nl::json::array(), nl::json::object()));
    }

    nl::json ghc_interpreter::complete_request_impl(const std::string& code,
                                                     int cursor_pos)
    {
        const auto result = ghc_protocol::normalize_completion(
            request_ghci("complete", code, cursor_pos), cursor_pos);
        return xeus::create_complete_reply(
            result.matches, result.cursor_start, result.cursor_end
        );
    }

    nl::json ghc_interpreter::inspect_request_impl(const std::string& code,
                                                    int cursor_pos,
                                                    int detail_level)
    {
        const auto result = ghc_protocol::normalize_inspection(
            request_ghci("inspect", code, cursor_pos, detail_level));
        if (!result.found)
        {
            return xeus::create_inspect_reply(false);
        }
        return xeus::create_inspect_reply(
            true, result.data, nl::json::object());
    }

    nl::json ghc_interpreter::is_complete_request_impl(const std::string& code)
    {
        const auto result = ghc_protocol::normalize_completeness(
            request_ghci("is_complete", code));
        return xeus::create_is_complete_reply(result.status, result.indent);
    }

    nl::json ghc_interpreter::kernel_info_request_impl()
    {
        return xeus::create_info_reply(
            "xhaskell-ghc", XEUS_HASKELL_VERSION, "haskell",
            "GHC " XEUS_HASKELL_GHC_VERSION, "text/x-haskell", ".hs",
            "haskell", std::string("haskell"), "module",
            "xhaskell-ghc: persistent GHCi running in the browser worker",
            nl::json::array()
        );
    }

    nl::json ghc_interpreter::shutdown_request_impl(bool restart)
    {
        return xeus::create_shutdown_reply(restart);
    }

    nl::json ghc_interpreter::interrupt_request_impl()
    {
        return xeus::create_interrupt_reply();
    }
}
