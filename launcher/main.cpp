#include <iostream>
#include <vector>
#include <ranges>

#include <windows.h>
#include <tlhelp32.h>
#include <shlobj.h>
#include <tchar.h>
#include <psapi.h>

#include "strutils.h"

#define MAX_CMDLINE_LENGTH 32768

#define GENERAL_SECTION _T("GENERAL")
#define ARGUMENTS_SECTION _T("ARGUMENTS")

bool switchPresents(std::vector<tstring> argv, tstring key) {
    for (auto s : argv) {
        if (s == key)
            return true;
    }

    return false;
}

void createDirectory(const TCHAR* path)
{
    TCHAR dir_name[MAX_PATH];
    const TCHAR* p = path;
    TCHAR* q = dir_name;

    while (*p)
    {
        if ((_T('\\') == *p) || (_T('/') == *p))
        {
            if (':' != *(p - 1))
            {
                CreateDirectory(dir_name, NULL);
            }
        }

        *q++ = *p++;
        *q = '\0';
    }

    CreateDirectory(dir_name, NULL);
}

void deleteDesktopLockFile(tstring homeDir)
{
    tstring lockFile = homeDir + _T("\\.emacs.d\\.emacs.desktop.lock");
    DeleteFile(lockFile.c_str());
}

bool checkProcess(PROCESSENTRY32 &entry, const wchar_t *processName) {
    bool exists = false;

    HANDLE hProcess = OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, entry.th32ProcessID);
    if (hProcess != NULL) {
        TCHAR filePath[MAX_PATH];
        if (GetModuleFileNameEx(hProcess, NULL, filePath, MAX_PATH)) {
            if (!_tcsicmp(filePath, processName))
                exists = true;
        }
        CloseHandle(hProcess);
    }

    return exists;
}

bool isProcessRunning(const wchar_t *processName)
{
    bool exists = false;
    PROCESSENTRY32 entry;
    entry.dwSize = sizeof(PROCESSENTRY32);

    HANDLE snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

    if (Process32First(snapshot, &entry) && !checkProcess(entry, processName))
        while (Process32Next(snapshot, &entry)) {
            exists = checkProcess(entry, processName);

            if (exists)
                break;
        }
    else {
        exists = true;
    }

    CloseHandle(snapshot);
    return exists;
}

DWORD launchTarget(tstring home, tstring target, tstring arguments, bool show, bool wait) {

    tstring serverFile = home + _T("\\.emacs.d");

    DWORD attrs = GetFileAttributes(serverFile.c_str());

    if (attrs == INVALID_FILE_ATTRIBUTES)
    {
        createDirectory(serverFile.c_str());
    }

    serverFile += _T("\\server");

    attrs = GetFileAttributes(serverFile.c_str());

    if (attrs == INVALID_FILE_ATTRIBUTES)
    {
        createDirectory(serverFile.c_str());
    }

    serverFile += _T("\\server");

    SetEnvironmentVariable(_T("EMACS_SERVER_FILE"), serverFile.c_str());

    STARTUPINFOW siStartupInfo;
    PROCESS_INFORMATION piProcessInfo;
    ZeroMemory(&siStartupInfo, sizeof(siStartupInfo));
    ZeroMemory(&piProcessInfo, sizeof(piProcessInfo));
    siStartupInfo.cb = sizeof(siStartupInfo);

    arguments = tstring(_T("\"")) + target + _T("\" ") + arguments;

    TCHAR cmdline[MAX_CMDLINE_LENGTH];
    _tcscpy(cmdline, arguments.c_str());

    BOOL success = CreateProcess(target.c_str(), cmdline, NULL, NULL, TRUE, (show ? 0 : CREATE_NO_WINDOW), NULL, NULL,
                                 &siStartupInfo, &piProcessInfo);

    if (success) {
        if (wait)
            WaitForSingleObject(piProcessInfo.hProcess, INFINITE);

        CloseHandle(piProcessInfo.hThread);
        CloseHandle(piProcessInfo.hProcess);
        return 0;
    }

    return 3;
}


int entry(HINSTANCE hInstance, std::vector<tstring> argv) {

    tstring moduleName = GetStringFromWindowsApi<TCHAR>([hInstance](TCHAR* buffer, int size) {
        return GetModuleFileName(hInstance, buffer, size);
    }, MAX_PATH);

    tstring moduleDir = moduleName.substr(0, moduleName.find_last_of(_T('\\')));
    SetEnvironmentVariable(_T("RHO_DIR"), moduleDir.c_str());

    tstring emacsDir = moduleDir + _T("\\emacs");
    SetEnvironmentVariable(_T("EMACS_DIR"), emacsDir.c_str());

    tstring emacsExecutable = emacsDir + _T("\\bin\\emacs.exe");

    tstring envPath = GetStringFromWindowsApi<TCHAR>([](TCHAR* buffer, int size) {
        return GetEnvironmentVariable(_T("PATH"), buffer, size);
    });

    envPath = moduleDir + _T("\\java\\bin;") + envPath + _T(";");

    SetEnvironmentVariable(_T("PATH"), envPath.c_str());

    tstring configPath = moduleName.substr(0, moduleName.size() - 3) + _T("conf");
    tstring targetKey = _T("TARGET");

    if (!isProcessRunning(emacsExecutable.c_str()))
        targetKey = _T("TARGET:FALLBACK");

    for (const auto &s : argv) {
        if (s.starts_with(_T("/TARGET:"))) {
            targetKey = s.substr(1);
            break;
        }
    }

    SetEnvironmentVariable(_T("INVOCATION_TAG"), targetKey.c_str());

    tstring targetPath = GetStringFromWindowsApi<TCHAR>([configPath, targetKey](TCHAR* buffer, int size) {
        return GetPrivateProfileString(GENERAL_SECTION, targetKey.c_str(), NULL, buffer, size, configPath.c_str());
    });

    if (targetPath.empty()) {
        return 2;
    }

    if (targetPath.find_first_of(_T(':')) == tstring::npos)
        targetPath = moduleDir + _T("/") + targetPath;

    tstring homeDir = GetStringFromWindowsApi<TCHAR>([configPath, targetKey](TCHAR* buffer, int size) {
        return GetPrivateProfileString(GENERAL_SECTION, _T("HOME"), NULL, buffer, size, configPath.c_str());
    });

    if (!homeDir.empty()) {
        if (homeDir.find_first_of(_T(':')) == tstring::npos)
            homeDir = moduleDir + _T("/") + homeDir;
    }
    else {
        TCHAR buff[MAX_PATH];
        SHGetFolderPath(NULL, CSIDL_PERSONAL, NULL, 0, buff);
        homeDir = buff;
        homeDir += _T("/rho-emacs");
    }

    for (const auto &s : argv) {
        if (s.starts_with(_T("/HOME:"))) {
            tstring argHome = s.substr(6);
            if (!argHome.empty())
                homeDir = argHome;
        }
        else if (s == _T("/GUEST")) {
            SetEnvironmentVariable(_T("RHO_GUEST"), _T("t"));
        }
    }

    SetEnvironmentVariable(_T("HOME"), homeDir.c_str());

    tstring firstFile;

    for (const auto &s : argv) {
        if (!s.starts_with(_T("/")) && !s.starts_with(_T("-"))) {
            firstFile = s;
            break;
        }
    }

    tstring fullFile = firstFile;

    if (!fullFile.empty() && fullFile.find_first_of(_T(':')) == tstring::npos) {
        tstring curDir = GetStringFromWindowsApi<TCHAR>([](TCHAR* buffer, int size) {
            return GetCurrentDirectory(size, buffer);
        }, MAX_PATH);

        fullFile = curDir + _T('/') + fullFile;
    }

    tstring argsToPass = GetStringFromWindowsApi<TCHAR>([configPath, targetKey](TCHAR* buffer, int size) {
        return GetPrivateProfileString(ARGUMENTS_SECTION, targetKey.c_str(), NULL, buffer, size, configPath.c_str());
    });

    if (argsToPass.empty())
        argsToPass = GetStringFromWindowsApi<TCHAR>([configPath, targetKey](TCHAR* buffer, int size) {
            return GetPrivateProfileString(ARGUMENTS_SECTION, _T(":FALLBACK"), NULL, buffer, size, configPath.c_str());
        });

    if (!firstFile.empty()) {
        auto pos = argsToPass.find(_T("%1"));
        if (pos != tstring::npos)
            argsToPass.replace(pos, 2, fullFile);
    }
    else {
        auto pos = argsToPass.find(_T("%1"));
        if (pos != tstring::npos)
            argsToPass.replace(pos, 4, _T(""));
    }

    auto argFilter = [firstFile, moduleName](tstring s)
            {return !s.starts_with(_T("/")) && !iequals(s, firstFile);};
    auto restArgs = argv | std::views::filter(argFilter);

    for (const auto &s : restArgs)
        argsToPass += tstring(_T(" \"")) + s + _T("\"");

    bool show = switchPresents(argv, _T("/SHOW"));

#ifdef RHOC
    show = targetKey == _T("TARGET:FALLBACK");
#endif

    bool wait = switchPresents(argv, _T("/WAIT"));

#ifdef RHOC
    wait = true;
#endif

    if (switchPresents(argv, _T("/TARGET:PRECOMP"))) {
        deleteDesktopLockFile(homeDir);
    }

    return launchTarget(homeDir, targetPath, argsToPass, show, wait);
}


int _tWinMain(HINSTANCE hInstance,
            HINSTANCE hPrevInstance,
            LPTSTR lpCmdLine,
            int nCmdShow)
{

    int argc = 0;
    LPTSTR *args = CommandLineToArgvW(GetCommandLine(), &argc);

    std::vector<tstring> argv(argc);

    for (int i = 1; i < argc; ++i)
        argv[i] = args[i];

    return entry(hInstance, argv);
}

