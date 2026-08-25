/***************************************************************************
 * Copyright (c) 2025, Masaya Taniguchi
 * Distributed under the terms of the Apache Software License 2.0.
 ****************************************************************************/

#ifndef XEUS_HASKELL_GHC_INTERPRETER_HPP
#define XEUS_HASKELL_GHC_INTERPRETER_HPP

#include <string>
#include "nlohmann/json.hpp"
#include "xeus/xinterpreter.hpp"

namespace nl = nlohmann;

namespace xeus_haskell
{
    class ghc_interpreter : public xeus::xinterpreter
    {
    public:
        ghc_interpreter();
        virtual ~ghc_interpreter() = default;

    private:
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
        nl::json shutdown_request_impl(bool restart) override;
        nl::json interrupt_request_impl() override;
    };
}

#endif
