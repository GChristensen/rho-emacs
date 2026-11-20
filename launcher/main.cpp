// ρEmacs launcher.
//
// Starts Emacs when no server is running for the selected home directory and
// acts like emacsclient otherwise. Behavior is configured by rho.conf (rhoc.conf
// for the console build), located next to the executable.

#include <map>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <vector>

#include "args.h"
#include "config.h"
#include "server.h"
#include "winutil.h"

#include <shellapi.h>

#ifdef RHOC
inline constexpr bool kConsoleBuild = true;
#else
inline constexpr bool kConsoleBuild = false;
#endif

namespace rho {
namespace {

constexpr std::wstring_view kGeneral = L"GENERAL";
constexpr std::wstring_view kArguments = L"ARGUMENTS";

constexpr std::wstring_view kKeyClient = L"TARGET";
constexpr std::wstring_view kKeyFallback = L"TARGET:FALLBACK";
constexpr std::wstring_view kKeyPrecomp = L"TARGET:PRECOMP";

constexpr DWORD kDefaultServerTimeoutMs = 15000;
constexpr DWORD kRaiseFrameTimeoutMs = 1500;
constexpr DWORD kRaiseDelayMs = 300;    // give a new frame a moment to appear
constexpr DWORD kLockMarginMs = 5000;   // lock waiters outlast the starter's wait

// ---------------------------------------------------------------- environment

struct CaseInsensitiveLess {
    bool operator()(const std::wstring& a, const std::wstring& b) const {
        return CompareStringOrdinal(a.c_str(), -1, b.c_str(), -1, TRUE) == CSTR_LESS_THAN;
    }
};
using EnvMap = std::map<std::wstring, std::wstring, CaseInsensitiveLess>;

EnvMap currentEnvironment() {
    EnvMap env;
    wchar_t* block = GetEnvironmentStringsW();
    if (!block)
        return env;
    for (const wchar_t* p = block; *p; p += wcslen(p) + 1) {
        std::wstring_view entry(p);
        // Entries such as "=C:=C:\dir" start with '='.
        size_t eq = entry.find(L'=', entry.starts_with(L'=') ? 1 : 0);
        if (eq == std::wstring_view::npos)
            continue;
        env[std::wstring(entry.substr(0, eq))] = std::wstring(entry.substr(eq + 1));
    }
    FreeEnvironmentStringsW(block);
    return env;
}

std::wstring environmentBlock(const EnvMap& env) {
    std::wstring block;
    for (const auto& [name, value] : env) {
        block += name;
        block += L'=';
        block += value;
        block += L'\0';
    }
    block += L'\0';
    return block;
}

// -------------------------------------------------------------------- helpers

DWORD parseMilliseconds(const std::optional<std::wstring>& text, DWORD fallback) {
    if (!text || text->empty())
        return fallback;
    wchar_t* end = nullptr;
    unsigned long v = wcstoul(text->c_str(), &end, 10);
    return (end && *end == L'\0') ? static_cast<DWORD>(v) : fallback;
}

bool configFlag(const Config& cfg, std::wstring_view key, bool fallback) {
    auto v = cfg.get(kGeneral, key);
    if (!v || v->empty())
        return fallback;
    return !(*v == L"0" || equalsNoCase(*v, L"false") || equalsNoCase(*v, L"no") || equalsNoCase(*v, L"off"));
}

Result<UniqueHandle> spawn(const fs::path& target, const std::wstring& args, const EnvMap& env,
                           bool show, const fs::path& workDir) {
    std::wstring commandLine = quoteArg(target.wstring());
    if (!args.empty()) {
        commandLine += L' ';
        commandLine += args;
    }
    logLine(L"spawn: " + commandLine);

    std::wstring envBlock = environmentBlock(env);
    std::vector<wchar_t> mutableCommandLine(commandLine.begin(), commandLine.end());
    mutableCommandLine.push_back(L'\0');

    STARTUPINFOW si{};
    si.cb = sizeof si;
    PROCESS_INFORMATION pi{};

    DWORD flags = CREATE_UNICODE_ENVIRONMENT | (show ? 0 : CREATE_NO_WINDOW);
    // The console build passes its stdio on to the child; the GUI build
    // must not leak handles into a long-living Emacs.
    BOOL inherit = kConsoleBuild ? TRUE : FALSE;

    if (!CreateProcessW(target.c_str(), mutableCommandLine.data(), nullptr, nullptr, inherit, flags,
                        envBlock.data(), workDir.empty() ? nullptr : workDir.c_str(), &si, &pi)) {
        return fail(ExitCode::SpawnFailed,
                    L"Cannot start " + target.wstring() + L":\n" + errorText(GetLastError()));
    }
    CloseHandle(pi.hThread);
    return makeHandle(pi.hProcess);
}

// ------------------------------------------------------------------ main logic

Result<int> run(std::span<const std::wstring> argv) {
    // --- locate ourselves and the configuration
    std::wstring moduleName = readWinString(
        [](wchar_t* buf, DWORD size) { return GetModuleFileNameW(nullptr, buf, size); });
    if (moduleName.empty())
        return fail(ExitCode::Internal, L"Cannot determine the launcher location:\n" + errorText(GetLastError()));

    const fs::path modulePath(moduleName);
    const fs::path moduleDir = modulePath.parent_path();
    const fs::path emacsDir = moduleDir / L"emacs";
    Config cfg(fs::path(modulePath).replace_extension(L".conf"));

    if (std::error_code ec; !fs::exists(cfg.file(), ec))
        return fail(ExitCode::BadConfig, L"Configuration file not found:\n" + cfg.file().wstring());

    const Options opts = parseArgs(argv);

    // --- home directory
    fs::path home;
    if (opts.home) {
        std::error_code ec;
        home = fs::absolute(fs::path(*opts.home), ec);
        if (ec)
            home = *opts.home;
    } else if (auto configured = cfg.get(kGeneral, L"HOME"); configured && !configured->empty()) {
        home = resolvePath(moduleDir, *configured);
    } else {
        auto def = defaultHome();
        if (!def)
            return std::unexpected(def.error());
        home = *def;
    }

    const fs::path serverDir = home / L".emacs.d" / L"server";
    {
        std::error_code ec;
        fs::create_directories(serverDir, ec);
        if (ec)
            return fail(ExitCode::BadConfig, L"Cannot create the directory " + serverDir.wstring() + L":\n" +
                                                 errorText(static_cast<DWORD>(ec.value())));
    }
    const fs::path serverFile = serverDir / L"server";

    const DWORD serverTimeout = parseMilliseconds(cfg.get(kGeneral, L"SERVER_TIMEOUT_MS"), kDefaultServerTimeoutMs);

    // --- decide which target to run
    const bool explicitTarget = opts.target.has_value();
    // Keys are upper-cased once so that all comparisons below are exact.
    std::wstring key = explicitTarget ? L"TARGET:" + toUpper(*opts.target) : std::wstring(kKeyClient);

    // Launchers that talk to (or start) the server are serialized per home.
    const bool usesServer = !explicitTarget || key == kKeyClient || key == L"TARGET:ORG_PROTOCOL" ||
                            key == kKeyFallback;

    WinsockSession winsock;
    std::optional<StartLock> startLock;
    std::optional<ServerInfo> server;

    if (usesServer) {
        if (!winsock)
            return fail(ExitCode::Internal, L"Cannot initialize Winsock.");
        auto lock = StartLock::acquire(serverFile, serverTimeout + kLockMarginMs);
        if (!lock)
            return std::unexpected(lock.error());
        startLock.emplace(std::move(*lock));

        server = findLiveServer(serverFile);
        if (!server && (!explicitTarget || (key != kKeyFallback && configFlag(cfg, L"FALLBACK_FOR_EXPLICIT", false))))
            key = kKeyFallback;
    }

    // --- resolve the program and its arguments
    auto targetValue = cfg.get(kGeneral, key);
    if (!targetValue || targetValue->empty())
        return fail(ExitCode::BadConfig,
                    L"The key \"" + key + L"\" is not defined in the [GENERAL] section of\n" + cfg.file().wstring());

    const fs::path targetPath = resolvePath(moduleDir, *targetValue);
    if (std::error_code ec; !fs::exists(targetPath, ec))
        return fail(ExitCode::BadConfig, L"The program configured for \"" + key + L"\" does not exist:\n" +
                                             targetPath.wstring());

    std::wstring rawArgs = cfg.get(kArguments, key).value_or(L"");
    if (rawArgs.empty())
        rawArgs = cfg.getAny(kArguments, {L":FALLBACK", L"FALLBACK"}).value_or(L"");

    std::wstring args = rawArgs;
    auto append = [&args](std::wstring_view piece) {
        if (!args.empty())
            args += L' ';
        args += piece;
    };

    // Make sure a frame is visible when the user starts rho while the server
    // is already running.
    if (key == kKeyClient && !kConsoleBuild && !selectsFrameMode(rawArgs)) {
        std::wstring mode = cfg.get(kGeneral, L"FRAME").value_or(L"reuse");
        if (opts.newFrame || equalsNoCase(mode, L"new"))
            append(L"-c");
        else if (!equalsNoCase(mode, L"none"))
            append(L"-r");
    }

    for (const auto& arg : opts.rest)
        for (const auto& piece : expandPositional(arg))
            append(quoteArg(piece));

    // --- environment of the child process
    EnvMap env = currentEnvironment();
    env[L"RHO_DIR"] = moduleDir.wstring();
    env[L"EMACS_DIR"] = emacsDir.wstring();
    env[L"HOME"] = home.wstring();
    env[L"EMACS_SERVER_FILE"] = serverFile.wstring();
    env[L"INVOCATION_TAG"] = key;
    if (opts.guest)
        env[L"RHO_GUEST"] = L"t";
    {
        std::wstring path = (moduleDir / L"java" / L"bin").wstring();
        if (auto it = env.find(L"PATH"); it != env.end() && !it->second.empty())
            path += L";" + it->second;
        env[L"PATH"] = std::move(path);
    }

    if (key == kKeyPrecomp) {
        std::error_code ec;
        fs::remove(home / L".emacs.d" / L".emacs.desktop.lock", ec);
    }

    // --- run it
    const bool show = kConsoleBuild ? (key == kKeyFallback) : opts.show;
    const bool wait = kConsoleBuild ? true : opts.wait;
    const bool focus = !kConsoleBuild && key == kKeyClient && server && configFlag(cfg, L"FOCUS", true);

    if (focus)
        AllowSetForegroundWindow(server->pid);  // lets Emacs raise its frame

    // A freshly started Emacs begins in the home directory; clients keep the
    // caller's directory so that relative file names resolve as expected.
    auto process = spawn(targetPath, args, env, show, key == kKeyFallback ? home : fs::path());
    if (!process)
        return std::unexpected(process.error());

    if (key == kKeyFallback && startLock) {
        // Hold the lock until the new server is reachable, so that launchers
        // arriving in the meantime talk to it instead of starting another Emacs.
        if (!waitForServer(serverFile, process->get(), serverTimeout))
            logLine(L"the Emacs server did not become ready");
    }
    startLock.reset();

    if (focus) {
        // Raise as soon as the client is done or a short delay has passed; a
        // client that waits for the user to finish editing never exits early.
        WaitForSingleObject(process->get(), kRaiseDelayMs);
        raiseServerFrame(server->pid, kRaiseFrameTimeoutMs);
    }

    if (wait) {
        WaitForSingleObject(process->get(), INFINITE);
        DWORD code = 0;
        if (!GetExitCodeProcess(process->get(), &code))
            return static_cast<int>(ExitCode::Internal);
        return static_cast<int>(code);
    }
    return static_cast<int>(ExitCode::Ok);
}

int runAndReport(std::span<const std::wstring> argv) {
    auto result = run(argv);
    if (result)
        return *result;
    reportError(result.error().message);
    return static_cast<int>(result.error().code);
}

std::vector<std::wstring> commandLineArguments() {
    int argc = 0;
    std::unique_ptr<LPWSTR[], decltype(&LocalFree)> args(CommandLineToArgvW(GetCommandLineW(), &argc), &LocalFree);

    std::vector<std::wstring> argv;
    if (args)
        for (int i = 1; i < argc; ++i)
            argv.emplace_back(args[i]);
    return argv;
}

}  // namespace
}  // namespace rho

#ifdef RHOC
int wmain() {
    auto argv = rho::commandLineArguments();
    return rho::runAndReport(argv);
}
#else
int WINAPI wWinMain(HINSTANCE, HINSTANCE, PWSTR, int) {
    auto argv = rho::commandLineArguments();
    return rho::runAndReport(argv);
}
#endif
