#ifndef RHO_SERVER_H
#define RHO_SERVER_H

#include <optional>
#include <string>

#include "winutil.h"

namespace rho {

// Contents of the first line of an Emacs server file: "host:port pid".
struct ServerInfo {
    std::string host;
    std::string port;
    DWORD pid = 0;
};

// Initializes Winsock for the lifetime of the object.
class WinsockSession {
public:
    WinsockSession() { ok_ = WSAStartup(MAKEWORD(2, 2), &data_) == 0; }
    ~WinsockSession() { if (ok_) WSACleanup(); }
    WinsockSession(const WinsockSession&) = delete;
    WinsockSession& operator=(const WinsockSession&) = delete;
    explicit operator bool() const { return ok_; }
private:
    WSADATA data_{};
    bool ok_ = false;
};

// Name of the named mutex serializing launchers of one server file.
// Stable across builds (FNV-1a of the lowercased normalized path).
std::wstring startLockName(const fs::path& serverFile);

std::optional<ServerInfo> readServerFile(const fs::path& serverFile);

// True if something accepts TCP connections at the server's address.
bool probeServer(const ServerInfo& info, int timeoutMs = 300);

// Reads the server file and probes it.
std::optional<ServerInfo> findLiveServer(const fs::path& serverFile);

// Serializes "start Emacs" decisions between concurrent launchers that use
// the same server file, so that two quick launches do not start two Emacs
// instances (or send a request to a server that is still starting).
class StartLock {
public:
    static Result<StartLock> acquire(const fs::path& serverFile, DWORD timeoutMs);
    StartLock(StartLock&&) noexcept = default;
    StartLock& operator=(StartLock&&) noexcept = default;
    ~StartLock();
    void release();
private:
    explicit StartLock(UniqueHandle mutex) : mutex_(std::move(mutex)) {}
    UniqueHandle mutex_;
};

// Polls until the server accepts connections, the process exits, or the
// timeout elapses.
std::optional<ServerInfo> waitForServer(const fs::path& serverFile, HANDLE process, DWORD timeoutMs);

// Best effort: bring the top-level Emacs frame of process `pid` to the
// foreground. Waits up to `timeoutMs` for a frame to exist.
void raiseServerFrame(DWORD pid, DWORD timeoutMs);

}  // namespace rho

#endif  // RHO_SERVER_H
