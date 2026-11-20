#include "server.h"

#include <ws2tcpip.h>

#include <charconv>
#include <cstdint>
#include <cwchar>

namespace rho {

std::optional<ServerInfo> readServerFile(const fs::path& serverFile) {
    // Plain Win32 I/O: the file is tiny, and iostreams would pull in the C++
    // locale machinery, which needs <iostream> static initialization.
    UniqueHandle file = makeHandle(CreateFileW(serverFile.c_str(), GENERIC_READ,
                                               FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, nullptr,
                                               OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr));
    if (file.get() == INVALID_HANDLE_VALUE)
        return std::nullopt;

    char data[512];
    DWORD got = 0;
    if (!ReadFile(file.get(), data, sizeof data, &got, nullptr) || got == 0)
        return std::nullopt;

    std::string line(data, got);
    line = line.substr(0, line.find_first_of("\r\n"));
    while (!line.empty() && (line.back() == '\r' || line.back() == '\n'))
        line.pop_back();

    auto space = line.find(' ');
    if (space == std::string::npos)
        return std::nullopt;

    std::string address = line.substr(0, space);
    std::string pidText = line.substr(space + 1);

    auto colon = address.rfind(':');  // the host may be an IPv6 literal
    if (colon == std::string::npos || colon == 0 || colon + 1 >= address.size())
        return std::nullopt;

    ServerInfo info;
    info.host = address.substr(0, colon);
    info.port = address.substr(colon + 1);
    if (info.host.size() > 2 && info.host.front() == '[' && info.host.back() == ']')
        info.host = info.host.substr(1, info.host.size() - 2);

    unsigned long pid = 0;
    auto [ptr, ec] = std::from_chars(pidText.data(), pidText.data() + pidText.size(), pid);
    if (ec != std::errc{})
        return std::nullopt;
    info.pid = static_cast<DWORD>(pid);
    return info;
}

bool probeServer(const ServerInfo& info, int timeoutMs) {
    addrinfo hints{};
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_NUMERICHOST | AI_NUMERICSERV;

    addrinfo* result = nullptr;
    if (getaddrinfo(info.host.c_str(), info.port.c_str(), &hints, &result) != 0 || result == nullptr)
        return false;

    bool connected = false;
    for (addrinfo* ai = result; ai && !connected; ai = ai->ai_next) {
        SOCKET s = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
        if (s == INVALID_SOCKET)
            continue;

        u_long nonBlocking = 1;
        ioctlsocket(s, FIONBIO, &nonBlocking);

        int rc = connect(s, ai->ai_addr, static_cast<int>(ai->ai_addrlen));
        if (rc == 0) {
            connected = true;
        } else if (WSAGetLastError() == WSAEWOULDBLOCK) {
            fd_set writeSet, errorSet;
            FD_ZERO(&writeSet);
            FD_ZERO(&errorSet);
            FD_SET(s, &writeSet);
            FD_SET(s, &errorSet);
            timeval tv{timeoutMs / 1000, (timeoutMs % 1000) * 1000};
            if (select(0, nullptr, &writeSet, &errorSet, &tv) > 0 && FD_ISSET(s, &writeSet))
                connected = true;
        }
        closesocket(s);
    }
    freeaddrinfo(result);
    return connected;
}

namespace {

// False only when the process is known to be gone. If it cannot be inspected
// (e.g. access denied) we give the benefit of the doubt.
bool processAlive(DWORD pid) {
    if (pid == 0)
        return true;
    UniqueHandle h = makeHandle(OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, pid));
    if (!h)
        return GetLastError() != ERROR_INVALID_PARAMETER;
    DWORD code = 0;
    return !GetExitCodeProcess(h.get(), &code) || code == STILL_ACTIVE;
}

}  // namespace

std::optional<ServerInfo> findLiveServer(const fs::path& serverFile) {
    auto info = readServerFile(serverFile);
    if (info && processAlive(info->pid) && probeServer(*info))
        return info;
    return std::nullopt;
}

std::wstring startLockName(const fs::path& serverFile) {
    std::wstring key = serverFile.lexically_normal().wstring();
    if (!key.empty())
        CharLowerBuffW(key.data(), static_cast<DWORD>(key.size()));

    uint64_t hash = 14695981039346656037ull;  // FNV-1a, 64 bit
    for (wchar_t c : key) {
        hash ^= static_cast<uint64_t>(c);
        hash *= 1099511628211ull;
    }
    wchar_t hex[17];
    swprintf(hex, 17, L"%016llx", static_cast<unsigned long long>(hash));
    return std::wstring(L"Local\\rho-emacs-start-") + hex;
}

Result<StartLock> StartLock::acquire(const fs::path& serverFile, DWORD timeoutMs) {
    std::wstring name = startLockName(serverFile);

    UniqueHandle mutex = makeHandle(CreateMutexW(nullptr, FALSE, name.c_str()));
    if (!mutex)
        return fail(ExitCode::Internal, L"Cannot create the launcher lock: " + errorText(GetLastError()));

    DWORD rc = WaitForSingleObject(mutex.get(), timeoutMs);
    if (rc == WAIT_OBJECT_0 || rc == WAIT_ABANDONED)
        return StartLock(std::move(mutex));

    return fail(ExitCode::ServerTimeout,
                L"Timed out waiting for another ρEmacs launcher that is starting Emacs.");
}

void StartLock::release() {
    if (mutex_) {
        ReleaseMutex(mutex_.get());
        mutex_.reset();
    }
}

StartLock::~StartLock() { release(); }

std::optional<ServerInfo> waitForServer(const fs::path& serverFile, HANDLE process, DWORD timeoutMs) {
    const ULONGLONG deadline = GetTickCount64() + timeoutMs;
    for (;;) {
        if (auto info = findLiveServer(serverFile))
            return info;
        if (process && WaitForSingleObject(process, 0) == WAIT_OBJECT_0)
            return std::nullopt;  // Emacs exited before starting the server
        if (GetTickCount64() >= deadline)
            return std::nullopt;
        Sleep(100);
    }
}

namespace {

struct FrameSearch {
    DWORD pid;
    HWND found = nullptr;
};

BOOL CALLBACK findFrame(HWND hwnd, LPARAM lParam) {
    auto* search = reinterpret_cast<FrameSearch*>(lParam);

    DWORD windowPid = 0;
    GetWindowThreadProcessId(hwnd, &windowPid);
    if (windowPid != search->pid)
        return TRUE;
    if (GetWindow(hwnd, GW_OWNER) != nullptr)
        return TRUE;
    if (!IsWindowVisible(hwnd))
        return TRUE;

    wchar_t className[16] = {};
    GetClassNameW(hwnd, className, 16);
    if (wcscmp(className, L"Emacs") != 0)
        return TRUE;

    search->found = hwnd;
    return FALSE;
}

}  // namespace

void raiseServerFrame(DWORD pid, DWORD timeoutMs) {
    const ULONGLONG deadline = GetTickCount64() + timeoutMs;
    for (;;) {
        FrameSearch search{pid};
        EnumWindows(findFrame, reinterpret_cast<LPARAM>(&search));
        if (search.found) {
            if (IsIconic(search.found))
                ShowWindow(search.found, SW_RESTORE);
            SetForegroundWindow(search.found);
            return;
        }
        if (GetTickCount64() >= deadline)
            return;
        Sleep(50);
    }
}

}  // namespace rho
