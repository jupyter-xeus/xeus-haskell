/***************************************************************************
 * Copyright (c) 2025, Masaya Taniguchi
 *
 * Distributed under the terms of the Apache Software License 2.0.
 *
 * The full license is in the file LICENSE, distributed with this software.
 ****************************************************************************/

#ifndef XEUS_HASKELL_HISTORY_MANAGER_HPP
#define XEUS_HASKELL_HISTORY_MANAGER_HPP

#include "nlohmann/json.hpp"
#include "xeus/xhistory_manager.hpp"
#include <string>
#include <vector>

namespace xeus_haskell {
struct history_entry {
  int session;
  int line_number;
  std::string input;
  std::string output;
};

class history_manager : public xeus::xhistory_manager {
public:
  history_manager() = default;
  virtual ~history_manager() = default;

protected:
  void configure_impl() override;
  void store_inputs_impl(int session, int line_num, const std::string &input,
                         const std::string &output) override;

  nlohmann::json get_tail_impl(int n, bool raw, bool output) const override;
  nlohmann::json get_range_impl(int session, int start, int stop, bool raw,
                                bool output) const override;
  nlohmann::json search_impl(const std::string &pattern, bool raw, bool output,
                             int n, bool unique) const override;

private:
  std::vector<history_entry> m_history;
};
} // namespace xeus_haskell

#endif
