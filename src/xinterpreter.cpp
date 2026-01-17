/***************************************************************************
 * Copyright (c) 2025, Masaya Taniguchi
 *
 * Distributed under the terms of the Apache Software License 2.0.
 *
 * The full license is in the file LICENSE, distributed with this software.
 ****************************************************************************/

#include <algorithm>
#include <cctype>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

#include "nlohmann/json.hpp"

#include "xeus/xhelper.hpp"
#include "xeus/xinput.hpp"
#include "xeus/xinterpreter.hpp"

#include "xeus-haskell/xhistory_manager.hpp"
#include "xeus-haskell/xinterpreter.hpp"

namespace nl = nlohmann;

namespace xeus_haskell {

interpreter::interpreter() { xeus::register_interpreter(this); }

void interpreter::execute_request_impl(send_reply_callback cb,
                                       int execution_counter,
                                       const std::string &code,
                                       xeus::execute_request_config config,
                                       nl::json /*user_expressions*/) {
  auto exec_result = [&]() -> repl_result {
    try {
      return m_repl.execute(code);
    } catch (const std::exception &e) {
      return {false, std::string(), std::string(e.what())};
    } catch (...) {
      return {false, std::string(), std::string("Unknown MicroHs error")};
    }
  }();

  if (!exec_result.ok) {
    const std::string &error_msg = exec_result.error;
    const std::vector<std::string> traceback{error_msg};
    publish_execution_error("RuntimeError", error_msg, traceback);

    nl::json traceback_json = nl::json::array();
    traceback_json.push_back(error_msg);

    cb(xeus::create_error_reply(error_msg, "RuntimeError", traceback_json));
    return;
  }

  if (!config.silent) {
    const std::string &output = exec_result.output;
    if (!output.empty()) {
      nl::json pub_data;
      pub_data["text/plain"] = output;
      publish_execution_result(execution_counter, std::move(pub_data),
                               nl::json::object());
    }
  }

  cb(xeus::create_successful_reply(nl::json::array(), nl::json::object()));
}

void interpreter::configure_impl() {
  // `configure_impl` allows you to perform some operations
  // after the custom_interpreter creation and before executing any request.
  // This is optional, but can be useful;
  // you can for example initialize an engine here or redirect output.
}

nl::json interpreter::is_complete_request_impl(const std::string &code) {
  // Insert code here to validate the ``code``
  // and use `create_is_complete_reply` with the corresponding status
  // "unknown", "incomplete", "invalid", "complete"
  std::string status = m_repl.is_complete(code);
  return xeus::create_is_complete_reply(status /*status*/, "   " /*indent*/);
}

nl::json interpreter::complete_request_impl(const std::string &code,
                                            int cursor_pos) {
  auto is_ident_char = [](char c) {
    return std::isalnum(static_cast<unsigned char>(c)) || c == '_' || c == '\'';
  };

  const std::size_t code_size = code.size();
  const std::size_t cursor = static_cast<std::size_t>(
      std::max(0, std::min(cursor_pos, static_cast<int>(code_size))));

  std::size_t start = cursor;
  while (start > 0 && is_ident_char(code[start - 1])) {
    --start;
  }

  if (start == cursor && cursor > 0) {
    // Fallback in case locale-dependent isalnum disagrees with our identifier
    // check.
    const std::string before_cursor = code.substr(0, cursor);
    const std::string ident_chars = "abcdefghijklmnopqrstuvwxyz"
                                    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                    "0123456789"
                                    "_'";
    const auto last_non_ident = before_cursor.find_last_not_of(ident_chars);
    start = (last_non_ident == std::string::npos) ? 0 : last_non_ident + 1;
  }

  const std::string prefix = code.substr(start, cursor - start);
  const auto candidates = m_repl.completion_candidates();
  std::vector<std::string> matches;
  matches.reserve(candidates.size());

  for (const auto &cand : candidates) {
    if (cand.rfind(prefix, 0) == 0) {
      matches.push_back(cand);
    }
  }

  return xeus::create_complete_reply(matches,                 /*matches*/
                                     static_cast<int>(start), /*cursor_start*/
                                     static_cast<int>(cursor) /*cursor_end*/
  );
}

nl::json interpreter::inspect_request_impl(const std::string &code,
                                           int cursor_pos,
                                           int /*detail_level*/) {
  auto is_ident_char = [](char c) {
    return std::isalnum(static_cast<unsigned char>(c)) || c == '_' || c == '\'';
  };

  const std::size_t code_size = code.size();
  const std::size_t cursor = static_cast<std::size_t>(
      std::max(0, std::min(cursor_pos, static_cast<int>(code_size))));

  std::size_t start = cursor;
  while (start > 0 && is_ident_char(code[start - 1])) {
    --start;
  }
  std::size_t end = cursor;
  while (end < code_size && is_ident_char(code[end])) {
    ++end;
  }

  const std::string name = code.substr(start, end - start);
  if (name.empty()) {
    return xeus::create_inspect_reply(false);
  }

  std::string info = m_repl.inspect(name);
  if (info.empty()) {
    return xeus::create_inspect_reply(false);
  }

  nl::json data;
  data["text/plain"] = info;

  return xeus::create_inspect_reply(true, data, data);
}

void interpreter::shutdown_request_impl() { std::cout << "Bye!!" << std::endl; }

nl::json interpreter::kernel_info_request_impl() {

  const std::string protocol_version = "5.3";
  const std::string implementation = "xhaskell";
  const std::string implementation_version = XEUS_HASKELL_VERSION;
  const std::string language_name = "haskell";
  const std::string language_version = XEUS_HASKELL_MICROHS_VERSION;
  const std::string language_mimetype = "text/x-haskell";
  ;
  const std::string language_file_extension = "hs";
  ;
  const std::string language_pygments_lexer = "";
  const std::string language_codemirror_mode = "";
  const std::string language_nbconvert_exporter = "";
  const std::string banner = "xhaskell";
  const bool debugger = false;

  const nl::json help_links = nl::json::array();

  return xeus::create_info_reply(
      protocol_version, implementation, implementation_version, language_name,
      language_version, language_mimetype, language_file_extension,
      language_pygments_lexer, language_codemirror_mode,
      language_nbconvert_exporter, banner, debugger, help_links);
}

} // namespace xeus_haskell
