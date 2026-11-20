#include "args.h"

#include <algorithm>
#include <ranges>

namespace rho {

namespace {

constexpr std::wstring_view kHomePrefix = L"/HOME:";
constexpr std::wstring_view kTargetPrefix = L"/TARGET:";

bool allDigits(std::wstring_view s) {
    return !s.empty() && std::ranges::all_of(s, [](wchar_t c) { return c >= L'0' && c <= L'9'; });
}

int hexValue(wchar_t c) {
    if (c >= L'0' && c <= L'9') return c - L'0';
    if (c >= L'a' && c <= L'f') return c - L'a' + 10;
    if (c >= L'A' && c <= L'F') return c - L'A' + 10;
    return -1;
}

// Decodes %XX sequences (UTF-8) of a URL path component.
std::wstring percentDecode(std::wstring_view in) {
    std::string bytes;
    bytes.reserve(in.size());
    for (size_t i = 0; i < in.size(); ++i) {
        wchar_t c = in[i];
        if (c == L'%' && i + 2 < in.size() && hexValue(in[i + 1]) >= 0 && hexValue(in[i + 2]) >= 0) {
            bytes.push_back(static_cast<char>(hexValue(in[i + 1]) * 16 + hexValue(in[i + 2])));
            i += 2;
        } else if (c < 0x80) {
            bytes.push_back(static_cast<char>(c));
        } else {
            // Non-ASCII literal: re-encode the whole run of such units as UTF-8,
            // so that surrogate pairs stay together.
            size_t end = i;
            while (end < in.size() && in[end] >= 0x80)
                ++end;
            int n = WideCharToMultiByte(CP_UTF8, 0, in.data() + i, static_cast<int>(end - i),
                                        nullptr, 0, nullptr, nullptr);
            if (n > 0) {
                size_t old = bytes.size();
                bytes.resize(old + static_cast<size_t>(n));
                WideCharToMultiByte(CP_UTF8, 0, in.data() + i, static_cast<int>(end - i),
                                    bytes.data() + old, n, nullptr, nullptr);
            }
            i = end - 1;
        }
    }
    if (bytes.empty())
        return {};
    int n = MultiByteToWideChar(CP_UTF8, 0, bytes.data(), static_cast<int>(bytes.size()), nullptr, 0);
    std::wstring out(n, L'\0');
    MultiByteToWideChar(CP_UTF8, 0, bytes.data(), static_cast<int>(bytes.size()), out.data(), n);
    return out;
}

}  // namespace

Options parseArgs(std::span<const std::wstring> argv) {
    Options opts;
    bool switchesDone = false;

    for (const auto& arg : argv) {
        if (switchesDone) {
            opts.rest.push_back(arg);
        } else if (arg == L"--") {
            switchesDone = true;
        } else if (startsWithNoCase(arg, kHomePrefix)) {
            if (arg.size() > kHomePrefix.size())
                opts.home = arg.substr(kHomePrefix.size());
        } else if (startsWithNoCase(arg, kTargetPrefix)) {
            if (arg.size() > kTargetPrefix.size())
                opts.target = arg.substr(kTargetPrefix.size());
        } else if (equalsNoCase(arg, L"/GUEST")) {
            opts.guest = true;
        } else if (equalsNoCase(arg, L"/SHOW")) {
            opts.show = true;
        } else if (equalsNoCase(arg, L"/WAIT")) {
            opts.wait = true;
        } else if (equalsNoCase(arg, L"/NEWFRAME")) {
            opts.newFrame = true;
        } else {
            opts.rest.push_back(arg);
        }
    }
    return opts;
}

std::wstring quoteArg(std::wstring_view arg) {
    if (!arg.empty() && arg.find_first_of(L" \t\n\v\"") == std::wstring_view::npos)
        return std::wstring(arg);

    std::wstring out = L"\"";
    size_t backslashes = 0;
    for (wchar_t c : arg) {
        if (c == L'\\') {
            ++backslashes;
            continue;
        }
        if (c == L'"') {
            out.append(backslashes * 2 + 1, L'\\');
            out += L'"';
        } else {
            out.append(backslashes, L'\\');
            out += c;
        }
        backslashes = 0;
    }
    out.append(backslashes * 2, L'\\');  // backslashes before the closing quote
    out += L'"';
    return out;
}

std::vector<std::wstring> expandPositional(const std::wstring& arg) {
    constexpr std::wstring_view fileUrl = L"file:///";

    if (arg.empty() || arg.front() == L'-' || arg.front() == L'+')
        return {arg};

    std::wstring_view rest = arg;
    bool wasUrl = false;
    std::wstring decoded;

    if (startsWithNoCase(rest, fileUrl)) {
        decoded = percentDecode(rest.substr(fileUrl.size()));
        std::ranges::replace(decoded, L'/', L'\\');
        // "file:///C:/x" -> "C:\x"; "file:///usr/x" keeps the leading slash.
        if (decoded.size() < 2 || decoded[1] != L':')
            decoded.insert(decoded.begin(), L'\\');
        rest = decoded;
        wasUrl = true;
    } else if (rest.find(L"://") != std::wstring_view::npos) {
        return {arg};  // org-protocol:// and friends
    }

    // Trailing ":line" or ":line:col".
    std::optional<std::wstring> position;
    std::wstring_view path = rest;
    if (auto p1 = path.rfind(L':'); p1 != std::wstring_view::npos && allDigits(path.substr(p1 + 1))) {
        std::wstring_view last = path.substr(p1 + 1);
        path = path.substr(0, p1);
        if (auto p2 = path.rfind(L':'); p2 != std::wstring_view::npos && allDigits(path.substr(p2 + 1))) {
            position = std::wstring(path.substr(p2 + 1)) + L":" + std::wstring(last);
            path = path.substr(0, p2);
        } else {
            position = std::wstring(last);
        }
        // Only treat it as a position if the file part really exists;
        // otherwise leave the argument alone (e.g. a new file named "a:1").
        std::error_code ec;
        if (path.empty() || !fs::exists(fs::path(path), ec)) {
            if (!wasUrl)
                return {arg};
            position.reset();
            path = rest;
        }
    }

    std::vector<std::wstring> out;
    if (position)
        out.push_back(L"+" + *position);
    out.emplace_back(path);
    return out;
}

bool selectsFrameMode(std::wstring_view rawArgs) {
    // Split on whitespace; compare whole tokens.
    size_t i = 0;
    while (i < rawArgs.size()) {
        while (i < rawArgs.size() && (rawArgs[i] == L' ' || rawArgs[i] == L'\t'))
            ++i;
        size_t start = i;
        while (i < rawArgs.size() && rawArgs[i] != L' ' && rawArgs[i] != L'\t')
            ++i;
        std::wstring_view tok = rawArgs.substr(start, i - start);
        // "--option=value" form.
        if (tok.starts_with(L"--"))
            tok = tok.substr(0, tok.find(L'='));
        if (tok == L"-c" || tok == L"-r" || tok == L"-t" || tok == L"-nw" || tok == L"-e" ||
            tok == L"--create-frame" || tok == L"--reuse-frame" || tok == L"--tty" || tok == L"--eval" ||
            tok == L"--no-window-system" || tok == L"-F" || tok == L"--frame-parameters")
            return true;
    }
    return false;
}

}  // namespace rho
