#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

#include "Repl_stub.h"
#include "mhs_stdout_capture.hpp"
#include "xeus-haskell/mhs_repl.hpp"

namespace xeus_haskell {

namespace {

bool mhs_repl_initialized = false;

struct ParsedOutput {
  std::string mime_type;
  std::string content;
};

ParsedOutput parse_protocol_output(std::string_view raw_output) {
  const char STX = '\x02';
  const char US = '\x1F';
  const char ETX = '\x03';

  const size_t us_pos = raw_output.find(US);
  if (us_pos == std::string_view::npos) {
    return {"text/plain", std::string(raw_output)};
  }

  std::string_view mime = raw_output.substr(0, us_pos);
  std::string_view content = raw_output.substr(us_pos + 1);

  if (!mime.empty() && mime.front() == STX) {
    mime.remove_prefix(1);
  } else {
    const size_t stx = mime.find(STX);
    if (stx != std::string_view::npos) {
      mime = mime.substr(stx + 1);
    }
  }

  while (!content.empty() &&
         (content.back() == ETX ||
          std::isspace(static_cast<unsigned char>(content.back())))) {
    content.remove_suffix(1);
  }

  return {std::string(mime), std::string(content)};
}

} // namespace

MicroHsRepl::MicroHsRepl() {
  if (!mhs_repl_initialized) {
    mhs_init();
    mhs_repl_initialized = true;
  }

  std::string microhs_runtime_dir;
  const auto* mhsdir = std::getenv("MHSDIR");
  const auto* conda_prefix = std::getenv("CONDA_PREFIX");

  if (mhsdir != nullptr) {
    microhs_runtime_dir = mhsdir;
  } else if (conda_prefix != nullptr) {
    microhs_runtime_dir = std::string(conda_prefix) + "/share/microhs";
  }

  context = mhs_repl_new(const_cast<char*>(microhs_runtime_dir.c_str()),
                         static_cast<uintptr_t>(microhs_runtime_dir.size()));

  auto warmup = execute("0");
  (void)warmup;
}

MicroHsRepl::~MicroHsRepl() { mhs_repl_free(context); }

repl_result MicroHsRepl::execute(std::string_view code) {
  std::string raw_output;

  try {
    raw_output = capture_stdout([&]() {
      std::string code_str(code);
      char* err = nullptr;
      intptr_t rc =
          mhs_repl_execute(context, const_cast<char*>(code_str.c_str()),
                           static_cast<uintptr_t>(code_str.size()),
                           reinterpret_cast<void*>(&err));

      if (rc != 0 && err) {
        std::string err_str(err);
        mhs_repl_free_cstr(err);
        throw std::runtime_error(err_str);
      }
      if (err) {
        mhs_repl_free_cstr(err);
      }
    });
  } catch (const std::runtime_error& e) {
    return {false, std::string(), std::string(e.what()), "text/plain"};
  }

  raw_output.erase(std::remove(raw_output.begin(), raw_output.end(), '\r'),
                   raw_output.end());

  ParsedOutput parsed = parse_protocol_output(raw_output);

  return {true, std::move(parsed.content), std::string(),
          std::move(parsed.mime_type)};
}

std::vector<std::string> MicroHsRepl::completion_candidates() {
  std::string output;
  try {
    output = capture_stdout([&]() { mhs_repl_completion_candidates(context); });
  } catch (const std::runtime_error&) {
    return {};
  }

  std::vector<std::string> candidates;
  std::stringstream ss(output);
  std::string line;
  while (std::getline(ss, line)) {
    if (!line.empty() && line.back() == '\r') {
      line.pop_back();
    }
    if (!line.empty()) {
      candidates.push_back(line);
    }
  }
  return candidates;
}

std::string MicroHsRepl::inspect(std::string_view name) {
  std::string name_str(name);
  char* info = nullptr;
  intptr_t rc = mhs_repl_inspect(context, const_cast<char*>(name_str.c_str()),
                                 static_cast<uintptr_t>(name_str.size()),
                                 reinterpret_cast<void*>(&info));

  if (rc == 0 && info) {
    std::string result(info);
    mhs_repl_free_cstr(info);
    return result;
  }
  if (info) {
    mhs_repl_free_cstr(info);
  }
  return "";
}

std::string MicroHsRepl::is_complete(std::string_view code) {
  std::string code_str(code);
  char* status = static_cast<char*>(
      mhs_repl_is_complete(context, const_cast<char*>(code_str.c_str()),
                           static_cast<uintptr_t>(code_str.size())));
  if (status) {
    std::string result(status);
    mhs_repl_free_cstr(status);
    return result;
  }
  return "unknown";
}

} // namespace xeus_haskell
