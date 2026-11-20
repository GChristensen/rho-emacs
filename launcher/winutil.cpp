#include "winutil.h"

#include <cstdio>

namespace rho {

std::wstring errorText(DWORD errorCode) {
    wchar_t* raw = nullptr;
    DWORD n = FormatMessageW(
        FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
        nullptr, errorCode, 0, reinterpret_cast<LPWSTR>(&raw), 0, nullptr);

    std::wstring text;
    if (n != 0 && raw != nullptr) {
        text.assign(raw, n);
        while (!text.empty() && (text.back() == L'\r' || text.back() == L'\n' || text.back() == L' '))
            text.pop_back();
    }
    if (raw)
        LocalFree(raw);

    text += L" (error " + std::to_wstring(errorCode) + L")";
    return text;
}

bool equalsNoCase(std::wstring_view a, std::wstring_view b) noexcept {
    if (a.size() != b.size())
        return false;
    if (a.empty())
        return true;
    return CompareStringOrdinal(a.data(), static_cast<int>(a.size()),
                                b.data(), static_cast<int>(b.size()), TRUE) == CSTR_EQUAL;
}

bool startsWithNoCase(std::wstring_view s, std::wstring_view prefix) noexcept {
    return s.size() >= prefix.size() && equalsNoCase(s.substr(0, prefix.size()), prefix);
}

std::wstring toUpper(std::wstring_view s) {
    std::wstring out(s);
    if (!out.empty())
        CharUpperBuffW(out.data(), static_cast<DWORD>(out.size()));
    return out;
}

void reportError(std::wstring_view message) {
    logLine(message);
#ifdef RHOC
    std::wstring line(message);
    line += L'\n';
    std::fputws(line.c_str(), stderr);
#else
    std::wstring text(message);
    MessageBoxW(nullptr, text.c_str(), L"ρEmacs launcher",
                MB_OK | MB_ICONERROR | MB_SETFOREGROUND);
#endif
}

std::wstring getEnv(const wchar_t* name) {
    return readWinString([name](wchar_t* buf, DWORD size) { return GetEnvironmentVariableW(name, buf, size); });
}

void logLine(std::wstring_view line) {
    std::wstring path = getEnv(L"RHO_LOG");
    if (path.empty())
        return;

    // Narrow to UTF-8 for the log file.
    std::string utf8;
    if (!line.empty()) {
        int n = WideCharToMultiByte(CP_UTF8, 0, line.data(), static_cast<int>(line.size()),
                                    nullptr, 0, nullptr, nullptr);
        utf8.resize(n);
        WideCharToMultiByte(CP_UTF8, 0, line.data(), static_cast<int>(line.size()),
                            utf8.data(), n, nullptr, nullptr);
    }

    utf8 += "\r\n";
    UniqueHandle file = makeHandle(CreateFileW(path.c_str(), FILE_APPEND_DATA,
                                               FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr, OPEN_ALWAYS,
                                               FILE_ATTRIBUTE_NORMAL, nullptr));
    if (file.get() == INVALID_HANDLE_VALUE)
        return;
    DWORD written = 0;
    WriteFile(file.get(), utf8.data(), static_cast<DWORD>(utf8.size()), &written, nullptr);
}

}  // namespace rho
