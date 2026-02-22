#ifdef _WIN32

#include "mhs_stdout_capture.hpp"

#include <cstdio>
#include <fcntl.h>
#include <io.h>
#include <stdexcept>
#include <windows.h>

namespace xeus_haskell {

std::string capture_stdout(const std::function<void()>& fn) {
  HANDLE hRead, hWrite;
  SECURITY_ATTRIBUTES sa = {sizeof(SECURITY_ATTRIBUTES), NULL, TRUE};

  if (!CreatePipe(&hRead, &hWrite, &sa, 0)) {
    throw std::runtime_error("Failed to create pipe");
  }

  HANDLE hWriteDup;
  if (!DuplicateHandle(GetCurrentProcess(), hWrite, GetCurrentProcess(),
                       &hWriteDup, 0, FALSE, DUPLICATE_SAME_ACCESS)) {
    CloseHandle(hRead);
    CloseHandle(hWrite);
    throw std::runtime_error("Failed to duplicate write handle");
  }

  HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
  int stdout_fd = _dup(1);
  if (stdout_fd == -1) {
    throw std::runtime_error("Failed to dup original stdout");
  }

  int pipe_fd = _open_osfhandle((intptr_t)hWriteDup, _O_TEXT);
  if (pipe_fd == -1) {
    _close(stdout_fd);
    CloseHandle(hRead);
    CloseHandle(hWrite);
    throw std::runtime_error("Failed to open osfhandle");
  }

  fflush(stdout);

  if (!SetStdHandle(STD_OUTPUT_HANDLE, hWrite)) {
    _close(pipe_fd);
    _close(stdout_fd);
    CloseHandle(hRead);
    CloseHandle(hWrite);
    throw std::runtime_error("Failed to set std handle");
  }

  if (_dup2(pipe_fd, 1) == -1) {
    SetStdHandle(STD_OUTPUT_HANDLE, hStdout);
    _close(pipe_fd);
    _close(stdout_fd);
    CloseHandle(hRead);
    CloseHandle(hWrite);
    throw std::runtime_error("Failed to dup2 stdout");
  }

  _close(pipe_fd);

  try {
    fn();
  } catch (...) {
    fflush(stdout);
    _dup2(stdout_fd, 1);
    _close(stdout_fd);
    SetStdHandle(STD_OUTPUT_HANDLE, hStdout);
    CloseHandle(hWrite);
    CloseHandle(hRead);
    throw;
  }

  fflush(stdout);
  _dup2(stdout_fd, 1);
  _close(stdout_fd);
  SetStdHandle(STD_OUTPUT_HANDLE, hStdout);
  CloseHandle(hWrite);

  std::string output;
  DWORD bytesRead;
  char buffer[4096];
  while (ReadFile(hRead, buffer, sizeof(buffer) - 1, &bytesRead, NULL) &&
         bytesRead > 0) {
    buffer[bytesRead] = '\0';
    output += buffer;
  }
  CloseHandle(hRead);
  return output;
}

} // namespace xeus_haskell

#endif
