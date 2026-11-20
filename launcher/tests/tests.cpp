// Tests of the pure helpers of the launcher (args.cpp).
#include <cstdio>
#include <cstring>
#include <memory>

#include "../args.h"
#include "../config.h"
#include "../server.h"

#include <shellapi.h>

using namespace rho;

static int failures = 0;

#define CHECK(cond)                                                     \
    do {                                                                \
        if (!(cond)) {                                                  \
            std::printf("FAIL %s:%d: %s\n", __FILE__, __LINE__, #cond); \
            ++failures;                                                 \
        }                                                               \
    } while (0)

using Strings = std::vector<std::wstring>;

// Parses a command line with the OS parser, the way every Windows program will.
static Strings osParse(const std::wstring& tail) {
    std::wstring line = L"prog.exe " + tail;
    int argc = 0;
    std::unique_ptr<LPWSTR[], decltype(&LocalFree)> a(CommandLineToArgvW(line.c_str(), &argc), &LocalFree);
    Strings out;
    for (int i = 1; i < argc; ++i)
        out.emplace_back(a[i]);
    return out;
}

static void testQuoting() {
    const Strings samples = {
        L"plain",
        L"with space",
        L"",
        L"C:\\dir with space\\",
        L"C:\\dir\\",
        L"a\"b",
        L"a\\\"b",
        L"\\\\server\\share\\",
        L"trailing\\\\",
        L"\"",
        L"\\",
        L"tab\there",
        L"unicode \u03C1 \u0416",
    };
    std::wstring line;
    for (const auto& s : samples)
        line += quoteArg(s) + L" ";
    CHECK(osParse(line) == samples);
}

static void testSwitches() {
    Strings argv = {L"/home:D:\\h", L"/GUEST", L"/c/foo.txt", L"/Show", L"-nw", L"a.txt"};
    Options o = parseArgs(argv);
    CHECK(o.home && *o.home == L"D:\\h");
    CHECK(o.guest && o.show && !o.wait);
    CHECK((o.rest == Strings{L"/c/foo.txt", L"-nw", L"a.txt"}));

    Strings argv2 = {L"/WAIT", L"--", L"/WAIT", L"/HOME:x"};
    Options o2 = parseArgs(argv2);
    CHECK(o2.wait && !o2.home);
    CHECK((o2.rest == Strings{L"/WAIT", L"/HOME:x"}));

    Strings argv3 = {L"/TARGET:PRECOMP", L"/NEWFRAME"};
    Options o3 = parseArgs(argv3);
    CHECK(o3.target && *o3.target == L"PRECOMP" && o3.newFrame);
}

static void testPositional() {
    // Nothing to expand.
    CHECK(expandPositional(L"-nw") == Strings{L"-nw"});
    CHECK(expandPositional(L"+12") == Strings{L"+12"});
    CHECK(expandPositional(L"org-protocol://store-link?url=x") == Strings{L"org-protocol://store-link?url=x"});
    CHECK(expandPositional(L"C:\\nonexistent\\file.txt:12") == Strings{L"C:\\nonexistent\\file.txt:12"});

    // Position suffixes for an existing path (the current directory).
    wchar_t cwd[MAX_PATH];
    GetCurrentDirectoryW(MAX_PATH, cwd);
    std::wstring dir = cwd;
    CHECK((expandPositional(dir + L":7") == Strings{L"+7", dir}));
    CHECK((expandPositional(dir + L":7:3") == Strings{L"+7:3", dir}));
    CHECK((expandPositional(dir) == Strings{dir}));

    // file:// URLs.
    CHECK((expandPositional(L"file:///C:/a%20b/c.txt") == Strings{L"C:\\a b\\c.txt"}));
}

static void testFrameMode() {
    CHECK(selectsFrameMode(L"-n -c"));
    CHECK(selectsFrameMode(L"--eval \"(x)\""));
    CHECK(!selectsFrameMode(L"-n"));
    CHECK(!selectsFrameMode(L""));
    CHECK(!selectsFrameMode(L"-nowait"));
}

static void testFrameModeEquals() {
    CHECK(selectsFrameMode(L"--eval=(x)"));
    CHECK(selectsFrameMode(L"--frame-parameters=((a . 1))"));
    CHECK(selectsFrameMode(L"--create-frame"));
    CHECK(!selectsFrameMode(L"--no-wait"));
}

static void testPercentSurrogates() {
    // 😀 U+1F600 as %-escapes and as a literal surrogate pair.
    CHECK((expandPositional(L"file:///C:/%F0%9F%98%80.txt") == Strings{L"C:\\\U0001F600.txt"}));
    CHECK((expandPositional(L"file:///C:/\U0001F600.txt") == Strings{L"C:\\\U0001F600.txt"}));
}

static std::wstring writeTemp(const char* content) {
    wchar_t dir[MAX_PATH], file[MAX_PATH];
    GetTempPathW(MAX_PATH, dir);
    GetTempFileNameW(dir, L"rho", 0, file);
    HANDLE h = CreateFileW(file, GENERIC_WRITE, 0, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);
    DWORD n = 0;
    WriteFile(h, content, static_cast<DWORD>(std::strlen(content)), &n, nullptr);
    CloseHandle(h);
    return file;
}

static void testServerFile() {
    struct Case { const char* text; bool ok; const char* host; const char* port; DWORD pid; };
    const Case cases[] = {
        {"127.0.0.1:1234 55\n\"key\"\n", true, "127.0.0.1", "1234", 55},
        {"127.0.0.1:1234 55\r\nkey", true, "127.0.0.1", "1234", 55},
        {"[::1]:9 7\n", true, "::1", "9", 7},
        {"127.0.0.1:1234\n", false, "", "", 0},
        {"garbage", false, "", "", 0},
        {"127.0.0.1:1234 xx\n", false, "", "", 0},
    };
    for (const auto& c : cases) {
        std::wstring f = writeTemp(c.text);
        auto info = readServerFile(f);
        CHECK(info.has_value() == c.ok);
        if (info && c.ok)
            CHECK(info->host == c.host && info->port == c.port && info->pid == c.pid);
        DeleteFileW(f.c_str());
    }
    CHECK(!readServerFile(L"C:\\definitely\\missing\\server"));
}

static void testLockName() {
    auto a = startLockName(L"C:\\Home\\.emacs.d\\server\\server");
    CHECK(a == startLockName(L"c:\\home\\.emacs.d\\server\\server"));
    CHECK(a != startLockName(L"C:\\Other\\server"));
    CHECK(a.starts_with(L"Local\\rho-emacs-start-") && a.size() == 22 + 16);
}

static void testResolvePath() {
    SetEnvironmentVariableW(L"RHO_TEST_DIR", L"D:\\x");
    CHECK(resolvePath(L"C:\\base", L"%RHO_TEST_DIR%\\y") == fs::path(L"D:\\x\\y"));
    CHECK(resolvePath(L"C:\\base", L"rel") == fs::path(L"C:\\base\\rel"));
    CHECK(resolvePath(L"C:\\base", L"D:\\abs") == fs::path(L"D:\\abs"));
    CHECK(toUpper(L"fallBack") == L"FALLBACK");
}

int wmain() {
    testFrameModeEquals();
    testPercentSurrogates();
    testServerFile();
    testLockName();
    testResolvePath();
    testQuoting();
    testSwitches();
    testPositional();
    testFrameMode();
    std::printf(failures ? "%d FAILURES\n" : "all tests passed\n", failures);
    return failures ? 1 : 0;
}
