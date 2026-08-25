#ifndef _WIN32

#include "mhs_stdout_capture.hpp"

#include <cstdio>
#include <unistd.h>

namespace xeus_haskell {

std::string capture_stdout(const std::function<void()>& fn) {
  int pipefd[2];
  if (pipe(pipefd) == -1) {
    return "";
  }

  int stdout_fd = dup(fileno(stdout));
  fflush(stdout);
  dup2(pipefd[1], fileno(stdout));
  close(pipefd[1]);

  fn();

  fflush(stdout);
  dup2(stdout_fd, fileno(stdout));
  close(stdout_fd);

  std::string output;
  char buffer[4096];
  ssize_t n;
  while ((n = read(pipefd[0], buffer, sizeof(buffer) - 1)) > 0) {
    buffer[n] = '\0';
    output += buffer;
  }
  close(pipefd[0]);
  return output;
}

} // namespace xeus_haskell

#endif
