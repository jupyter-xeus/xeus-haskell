/***************************************************************************
 * Copyright (c) 2025, Masaya Taniguchi
 *
 * Distributed under the terms of the Apache Software License 2.0.
 *
 * The full license is in the file LICENSE, distributed with this software.
 ****************************************************************************/

#include "xeus-haskell/xhistory_manager.hpp"
#include <algorithm>
#include <regex>
#include <set>

namespace xeus_haskell {

void history_manager::configure_impl() {
  // Nothing to configure for in-memory history
}

void history_manager::store_inputs_impl(int session, int line_num,
                                        const std::string &input,
                                        const std::string &output) {
  m_history.push_back({session, line_num, input, output});
}

nlohmann::json history_manager::get_tail_impl(int n, bool /*raw*/,
                                              bool /*output*/) const {
  nlohmann::json history = nlohmann::json::array();
  int count = 0;
  for (auto it = m_history.rbegin(); it != m_history.rend(); ++it) {
    history.push_back(nlohmann::json::array(
        {nlohmann::json(it->session), nlohmann::json(it->line_number),
         nlohmann::json(it->input)}));
    if (++count >= n)
      break;
  }
  // The history should be returned in chronological order
  std::reverse(history.begin(), history.end());
  return nlohmann::json{{"history", history}, {"status", "ok"}};
}

nlohmann::json history_manager::get_range_impl(int session, int start, int stop,
                                               bool /*raw*/,
                                               bool /*output*/) const {
  nlohmann::json history = nlohmann::json::array();
  for (const auto &entry : m_history) {
    if (entry.session == session) {
      if (entry.line_number >= start && entry.line_number < stop) {
        history.push_back(nlohmann::json::array(
            {nlohmann::json(entry.session), nlohmann::json(entry.line_number),
             nlohmann::json(entry.input)}));
      }
    }
  }
  return nlohmann::json{{"history", history}, {"status", "ok"}};
}

nlohmann::json history_manager::search_impl(const std::string &pattern,
                                            bool /*raw*/, bool /*output*/,
                                            int n, bool unique) const {
  nlohmann::json history = nlohmann::json::array();
  std::regex regex_pattern(pattern.empty() ? ".*" : pattern);
  std::set<std::string> seen_inputs;
  int count = 0;
  for (auto it = m_history.rbegin(); it != m_history.rend(); ++it) {
    if (std::regex_search(it->input, regex_pattern)) {
      if (unique) {
        if (seen_inputs.find(it->input) != seen_inputs.end()) {
          continue;
        }
        seen_inputs.insert(it->input);
      }
      history.push_back(nlohmann::json::array(
          {nlohmann::json(it->session), nlohmann::json(it->line_number),
           nlohmann::json(it->input)}));
      if (++count >= n)
        break;
    }
  }
  std::reverse(history.begin(), history.end());
  return nlohmann::json{{"history", history}, {"status", "ok"}};
}

} // namespace xeus_haskell
