#ifndef RHO_WINUTIL_H
#define RHO_WINUTIL_H

#include <winsock2.h>
#include <windows.h>

#include <expected>
#include <filesystem>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>

namespace rho {

namespace fs = std::filesystem;

// Process exit codes of the launcher itself.
enum class ExitCode : int {
    Ok = 0,
    BadConfig = 2,      // target is not configured / does not exist
    SpawnFailed = 3,    // CreateProcess failed
    ServerTimeout = 4,  // the Emacs server did not become ready in time
    Internal = 5,
};

struct Error {
    ExitCode code = ExitCode::Internal;
    std::wstring message;
};

template <typename T>
using Result = std::expected<T, Error>;

inline std::unexpected<Error> fail(ExitCode code, std::wstring message) {
    return std::unexpected(Error{code, std::move(message)});
}

// RAII wrapper for kernel HANDLEs.
struct HandleCloser {
    void operator()(HANDLE h) const noexcept {
        if (h && h != INVALID_HANDLE_VALUE)
            CloseHandle(h);
    }
};
using UniqueHandle = std::unique_ptr<std::remove_pointer_t<HANDLE>, HandleCloser>;

inline UniqueHandle makeHandle(HANDLE h) { return UniqueHandle(h); }

// Calls a Win32 "fill a buffer" API (returns the length written, 0 on failure)
// with a growing buffer until the result is not truncated.
template <typename Getter>
std::wstring readWinString(Getter&& getter, DWORD initialSize = MAX_PATH) {
    constexpr DWORD maxSize = 1u << 20;
    std::wstring buffer(initialSize, L'\0');
    for (;;) {
        DWORD n = getter(buffer.data(), static_cast<DWORD>(buffer.size()));
        if (n == 0)
            return {};
        // Truncated results are reported as size-1 (profile strings),
        // size (module names) or the required size (environment variables).
        if (n + 1 < buffer.size()) {
            buffer.resize(n);
            return buffer;
        }
        if (buffer.size() >= maxSize)
            return {};
        buffer.resize(buffer.size() * 2);
    }
}

// Human readable text of a Win32 error code.
std::wstring errorText(DWORD errorCode);

// Case-insensitive (ordinal) comparison, independent of the C locale.
bool equalsNoCase(std::wstring_view a, std::wstring_view b) noexcept;
bool startsWithNoCase(std::wstring_view s, std::wstring_view prefix) noexcept;

// Upper-cases a string (locale independent).
std::wstring toUpper(std::wstring_view s);

// Reports an error to the user: a message box in the GUI build,
// stderr in the console build.
void reportError(std::wstring_view message);

// Appends a line to the file named by %RHO_LOG% (no-op if unset).
void logLine(std::wstring_view line);

std::wstring getEnv(const wchar_t* name);

}  // namespace rho

#endif  // RHO_WINUTIL_H
