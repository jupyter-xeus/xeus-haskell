/***************************************************************************
* Copyright (c) 2025, Masaya Taniguchi                                  
*                                                                          
* Distributed under the terms of the Apache Software License 2.0.                 
*                                                                          
* The full license is in the file LICENSE, distributed with this software. 
****************************************************************************/

#include <string>
#include <vector>
#include <iostream>
#include <stdexcept>

#include "nlohmann/json.hpp"

#include "xeus/xinput.hpp"
#include "xeus/xinterpreter.hpp"
#include "xeus/xhelper.hpp"

#include "xeus-haskell/xinterpreter.hpp"

namespace nl = nlohmann;

namespace xeus_haskell
{
 
    interpreter::interpreter()
    {
        xeus::register_interpreter(this);
    }

    void interpreter::execute_request_impl(send_reply_callback cb,
                                           int execution_counter,
                                           const std::string& code,
                                           xeus::execute_request_config config,
                                           nl::json /*user_expressions*/)
    {
        auto exec_result = [&]() -> repl_result {
            try
            {
                return m_repl.execute(code);
            }
            catch (const std::exception& e)
            {
                return {false, std::string(), std::string(e.what())};
            }
            catch (...)
            {
                return {false, std::string(), std::string("Unknown MicroHs error")};
            }
        }();

        if (!exec_result.ok)
        {
            const std::string& error_msg = exec_result.error;
            const std::vector<std::string> traceback{error_msg};
            publish_execution_error("RuntimeError", error_msg, traceback);

            nl::json traceback_json = nl::json::array();
            traceback_json.push_back(error_msg);

            cb(xeus::create_error_reply(error_msg, "RuntimeError", traceback_json));
            return;
        }

        if (!config.silent)
        {
            const std::string& output = exec_result.output;
            if (!output.empty())
            {
                nl::json pub_data;
                pub_data[exec_result.mime_type] = output;
                publish_execution_result(execution_counter, std::move(pub_data), nl::json::object());
            }
        }

        cb(xeus::create_successful_reply(nl::json::array(), nl::json::object()));
    }

    void interpreter::configure_impl()
    {
        // `configure_impl` allows you to perform some operations
        // after the custom_interpreter creation and before executing any request.
        // This is optional, but can be useful;
        // you can for example initialize an engine here or redirect output.
    }

    nl::json interpreter::is_complete_request_impl(const std::string& code)
    {
        // Insert code here to validate the ``code``
        // and use `create_is_complete_reply` with the corresponding status
        // "unknown", "incomplete", "invalid", "complete"
        return xeus::create_is_complete_reply("complete"/*status*/, "   "/*indent*/);
    }

    nl::json interpreter::complete_request_impl(const std::string&  code,
                                                     int cursor_pos)
    {
        // Should be replaced with code performing the completion
        // and use the returned `matches` to `create_complete_reply`
        // i.e if the code starts with 'H', it could be the following completion
        if (code[0] == 'H')
        {
       
            return xeus::create_complete_reply(
                {
                    std::string("Hello"), 
                    std::string("Hey"), 
                    std::string("Howdy")
                },          /*matches*/
                5,          /*cursor_start*/
                cursor_pos  /*cursor_end*/
            );
        }

        // No completion result
        else
        {

            return xeus::create_complete_reply(
                nl::json::array(),  /*matches*/
                cursor_pos,         /*cursor_start*/
                cursor_pos          /*cursor_end*/
            );
        }
    }

    nl::json interpreter::inspect_request_impl(const std::string& /*code*/,
                                                      int /*cursor_pos*/,
                                                      int /*detail_level*/)
    {
        
        return xeus::create_inspect_reply(true/*found*/, 
            {{std::string("text/plain"), std::string("hello!")}}, /*data*/
            {{std::string("text/plain"), std::string("hello!")}}  /*meta-data*/
        );
         
    }

    void interpreter::shutdown_request_impl() {
        std::cout << "Bye!!" << std::endl;
    }

    nl::json interpreter::kernel_info_request_impl()
    {

        const std::string  protocol_version = "5.3";
        const std::string  implementation = "xhaskell";
        const std::string  implementation_version = XEUS_HASKELL_VERSION;
        const std::string  language_name = "haskell";
        const std::string  language_version = XEUS_HASKELL_MICROHS_VERSION;
        const std::string  language_mimetype = "text/x-haskell";;
        const std::string  language_file_extension = "hs";;
        const std::string  language_pygments_lexer = "";
        const std::string  language_codemirror_mode = "";
        const std::string  language_nbconvert_exporter = "";
        const std::string  banner = "xhaskell";
        const bool         debugger = false;
        
        const nl::json     help_links = nl::json::array();


        return xeus::create_info_reply(
            protocol_version,
            implementation,
            implementation_version,
            language_name,
            language_version,
            language_mimetype,
            language_file_extension,
            language_pygments_lexer,
            language_codemirror_mode,
            language_nbconvert_exporter,
            banner,
            debugger,
            help_links
        );
    }

}
