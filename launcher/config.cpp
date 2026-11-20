#include "config.h"

#include <shlobj.h>

namespace rho {

namespace {
// A value that cannot appear in a config file; used to tell "absent" from "empty".
constexpr wchar_t kAbsent[] = L"\x01<absent>\x01";
}  // namespace

std::optional<std::wstring> Config::get(std::wstring_view section, std::wstring_view key) const {
    std::wstring sec(section), k(key), path = file_.wstring();
    std::wstring value = readWinString([&](wchar_t* buf, DWORD size) {
        return GetPrivateProfileStringW(sec.c_str(), k.c_str(), kAbsent, buf, size, path.c_str());
    });
    if (value == kAbsent)
        return std::nullopt;
    return value;
}

std::optional<std::wstring> Config::getAny(std::wstring_view section,
                                           std::initializer_list<std::wstring_view> keys) const {
    for (auto key : keys)
        if (auto v = get(section, key))
            return v;
    return std::nullopt;
}

fs::path resolvePath(const fs::path& base, std::wstring_view value) {
    // Expand %VAR% references. Note: GetPrivateProfileString reads ANSI unless
    // the .conf file is UTF-16 LE, so non-ASCII paths need a UTF-16 config.
    std::wstring text(value);
    if (text.find(L'%') != std::wstring::npos) {
        std::wstring expanded(MAX_PATH, L'\0');
        DWORD n = ExpandEnvironmentStringsW(text.c_str(), expanded.data(), static_cast<DWORD>(expanded.size()));
        if (n > expanded.size()) {
            expanded.resize(n);
            n = ExpandEnvironmentStringsW(text.c_str(), expanded.data(), static_cast<DWORD>(expanded.size()));
        }
        if (n > 0 && n <= expanded.size()) {
            expanded.resize(n - 1);  // n includes the terminating null
            text = std::move(expanded);
        }
    }
    fs::path p{text};
    if (p.is_absolute())
        return p.lexically_normal();
    return (base / p).lexically_normal();
}

Result<fs::path> defaultHome() {
    PWSTR raw = nullptr;
    HRESULT hr = SHGetKnownFolderPath(FOLDERID_Documents, KF_FLAG_DEFAULT, nullptr, &raw);
    if (FAILED(hr) || raw == nullptr) {
        if (raw)
            CoTaskMemFree(raw);
        return fail(ExitCode::BadConfig, L"Cannot locate the Documents folder to use as the Emacs home directory.");
    }
    fs::path home = fs::path(raw) / L"rho-emacs";
    CoTaskMemFree(raw);
    return home;
}

}  // namespace rho
