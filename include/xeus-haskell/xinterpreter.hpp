/***************************************************************************
* Copyright (c) 2025, Masaya Taniguchi                                  
*                                                                          
* Distributed under the terms of the Apache Software License 2.0.                 
*                                                                          
* The full license is in the file LICENSE, distributed with this software. 
****************************************************************************/


#ifndef XEUS_HASKELL_INTERPRETER_HPP
#define XEUS_HASKELL_INTERPRETER_HPP

#ifdef __GNUC__
    #pragma GCC diagnostic push
    #pragma GCC diagnostic ignored "-Wattributes"
#endif

#include <string>
#include <memory>

#include "nlohmann/json.hpp"

#include "xeus_haskell_config.hpp"
#include "xeus/xinterpreter.hpp"
#include "xeus-haskell/mhs_repl.hpp"


namespace nl = nlohmann;

namespace xeus_haskell
{
    class XEUS_HASKELL_API interpreter : public xeus::xinterpreter
    {
    public:

        interpreter();
        virtual ~interpreter() = default;

    protected:

        void configure_impl() override;

        
        void execute_request_impl(send_reply_callback cb,
                                  int execution_counter,
                                  const std::string& code,
                                  xeus::execute_request_config config,
                                  nl::json user_expressions) override;

        nl::json complete_request_impl(const std::string& code, int cursor_pos) override;

        nl::json inspect_request_impl(const std::string& code,
                                      int cursor_pos,
                                      int detail_level) override;

        nl::json is_complete_request_impl(const std::string& code) override;

        nl::json kernel_info_request_impl() override;

        void shutdown_request_impl() override;

    private:
        MicroHsRepl m_repl;
    };
}

#ifdef __GNUC__
    #pragma GCC diagnostic pop
#endif

#endif
