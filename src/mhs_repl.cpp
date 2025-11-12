#include <expected>
#include <functional>
#include <string>
#include <string_view>
#include <regex>
#include <cstdio>

#ifdef _WIN32
#include <windows.h>
#include <io.h>
#include <fcntl.h>
#else
#include <unistd.h>
#include <fcntl.h>
#endif

#include "xeus-haskell/mhs_repl.hpp"
#include "Repl_stub.h"

namespace xeus_haskell {

namespace {

bool mhs_repl_initialized = false;

#ifdef MICROHS_RUNTIME_DIR
constexpr const char* microhs_runtime_dir = MICROHS_RUNTIME_DIR;
#else
constexpr const char* microhs_runtime_dir = nullptr;
#endif

void ensure_microhs_environment()
{
    if (microhs_runtime_dir == nullptr || microhs_runtime_dir[0] == '\0')
    {
        return;
    }

    const char* current = std::getenv("MHSDIR");
    if (current != nullptr && current[0] != '\0')
    {
        return;
    }

#ifdef _WIN32
    _putenv_s("MHSDIR", microhs_runtime_dir);
#else
    setenv("MHSDIR", microhs_runtime_dir, 1);
#endif
}

bool is_definition(std::string_view code) {
    std::string trimmed = std::regex_replace(std::string(code), std::regex(R"(--.*$)"), "");
    static const std::regex re(
        R"(^\s*(import|data|newtype|type|class|instance|foreign|infixl?|infixr|default)\b|(^|[^<>=:/!])=([^<>=:/!]|$)|::)"
    );
    return std::regex_search(trimmed, re);
}

std::string capture_stdout(std::function<void()> fn) {
#ifdef _WIN32
    // Windows implementation (Fixed to capture Win32 and C runtime stdout)
    HANDLE hRead, hWrite;
    SECURITY_ATTRIBUTES sa = { sizeof(SECURITY_ATTRIBUTES), NULL, TRUE };

    if (!CreatePipe(&hRead, &hWrite, &sa, 0)) {
        throw std::runtime_error("Failed to create pipe");
    }

    // Duplicate the write handle for the C runtime
    HANDLE hWriteDup;
    if (!DuplicateHandle(GetCurrentProcess(), hWrite, GetCurrentProcess(), &hWriteDup, 0, FALSE, DUPLICATE_SAME_ACCESS)) {
        CloseHandle(hRead);
        CloseHandle(hWrite);
        throw std::runtime_error("Failed to duplicate write handle");
    }

    // Save original stdout handles (both Win32 and C runtime)
    HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
    int stdout_fd = _dup(1); // Save original C runtime stdout (FD 1)
    if (stdout_fd == -1) {
         throw std::runtime_error("Failed to dup original stdout");
    }

    // Create a C runtime file descriptor for the duplicated pipe handle
    int pipe_fd = _open_osfhandle((intptr_t)hWriteDup, _O_TEXT);
    if (pipe_fd == -1) {
        _close(stdout_fd);
        CloseHandle(hRead);
        CloseHandle(hWrite);
        throw std::runtime_error("Failed to open osfhandle");
    }

    fflush(stdout); // Flush any existing buffers

    // 1. Redirect Win32 stdout
    if (!SetStdHandle(STD_OUTPUT_HANDLE, hWrite)) {
        _close(pipe_fd);
        _close(stdout_fd);
        CloseHandle(hRead);
        CloseHandle(hWrite);
        throw std::runtime_error("Failed to set std handle");
    }

    // 2. Redirect C runtime stdout
    if (_dup2(pipe_fd, 1) == -1) {
        // Restore Win32 handle before throwing
        SetStdHandle(STD_OUTPUT_HANDLE, hStdout);
        _close(pipe_fd);
        _close(stdout_fd);
        CloseHandle(hRead);
        CloseHandle(hWrite);
        throw std::runtime_error("Failed to dup2 stdout");
    }

    // We've duplicated pipe_fd onto 1, so we can close the original
    // This also closes the underlying hWriteDup handle
    _close(pipe_fd);

    try {
        fn();  // run the function
    } catch (...) {
        // Restore stdouts before re-throwing
        fflush(stdout);
        _dup2(stdout_fd, 1);
        _close(stdout_fd);
        SetStdHandle(STD_OUTPUT_HANDLE, hStdout);
        CloseHandle(hWrite);
        CloseHandle(hRead);
        throw; // Re-throw the original exception
    }

    // Restore original stdouts
    fflush(stdout);

    // 1. Restore C runtime stdout
    _dup2(stdout_fd, 1);
    _close(stdout_fd);

    // 2. Restore Win32 stdout
    SetStdHandle(STD_OUTPUT_HANDLE, hStdout);

    // Close the Win32 pipe write handle.
    // Reading can now begin.
    CloseHandle(hWrite);

    // Read from pipe
    std::string output;
    DWORD bytesRead;
    char buffer[4096];
    while (ReadFile(hRead, buffer, sizeof(buffer) - 1, &bytesRead, NULL) && bytesRead > 0) {
        buffer[bytesRead] = '\0';
        output += buffer;
    }
    CloseHandle(hRead);
    return output;
#else
    // POSIX implementation
    int pipefd[2];
    if (pipe(pipefd) == -1) return "";

    int stdout_fd = dup(fileno(stdout));
    fflush(stdout);
    dup2(pipefd[1], fileno(stdout));
    close(pipefd[1]);

    fn();  // execute function that writes to stdout

    fflush(stdout);
    dup2(stdout_fd, fileno(stdout));
    close(stdout_fd);

    // Read captured output
    std::string output;
    char buffer[4096];
    ssize_t n;
    while ((n = read(pipefd[0], buffer, sizeof(buffer) - 1)) > 0) {
        buffer[n] = '\0';
        output += buffer;
    }
    close(pipefd[0]);
    return output;
#endif
}
} // namespace

MicroHsRepl::MicroHsRepl() {
    ensure_microhs_environment();
    if (!mhs_repl_initialized) {
        mhs_init();
        mhs_repl_initialized = true;
    }
    context = mhs_repl_new();
}

MicroHsRepl::~MicroHsRepl() {
    mhs_repl_free(context);
}

std::expected<std::string, std::string> MicroHsRepl::execute(std::string_view code) {
    if (is_definition(code))
        return define(code);
    return run(code);
}

std::expected<std::string, std::string> MicroHsRepl::define(std::string_view code) {
    std::string output = capture_stdout([&]() {
        std::string code_str(code);
        char* err = nullptr;
        intptr_t rc = mhs_repl_define(
            context,
            const_cast<char*>(code_str.c_str()),
            static_cast<uintptr_t>(code_str.size()),
            reinterpret_cast<void*>(&err)
        );

        if (rc != 0 && err) {
            std::string err_str(err);
            mhs_repl_free_cstr(err);
            throw std::runtime_error(err_str);
        }
        if (err) mhs_repl_free_cstr(err);
    });

    return output;
}

std::expected<std::string, std::string> MicroHsRepl::run(std::string_view code) {
    std::string output;
    try {
        output = capture_stdout([&]() {
            std::string code_str(code);
            char* err = nullptr;
            intptr_t rc = mhs_repl_run(
                context,
                const_cast<char*>(code_str.c_str()),
                static_cast<uintptr_t>(code_str.size()),
                reinterpret_cast<void*>(&err)
            );

            if (rc != 0 && err) {
                std::string err_str(err);
                mhs_repl_free_cstr(err);
                throw std::runtime_error(err_str);
            }
            if (err) mhs_repl_free_cstr(err);
        });
    } catch (const std::runtime_error& e) {
        return std::unexpected(e.what());
    }
    return output;
}

} // namespace xeus_haskell

