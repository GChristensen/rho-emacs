#ifndef RHO_ARGS_H
#define RHO_ARGS_H

#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "winutil.h"

namespace rho {

// Launcher switches (everything else is passed through to Emacs).
//   /HOME:dir  /TARGET:key  /GUEST  /SHOW  /WAIT  /NEWFRAME
// "--" ends launcher switch processing.
struct Options {
    std::optional<std::wstring> home;
    std::optional<std::wstring> target;
    bool guest = false;
    bool show = false;
    bool wait = false;
    bool newFrame = false;
    std::vector<std::wstring> rest;  // arguments for the target program
};

Options parseArgs(std::span<const std::wstring> argv);

// Quotes one argument according to the rules of CommandLineToArgvW.
std::wstring quoteArg(std::wstring_view arg);

// Expands "file:line[:col]" and "file:///path" arguments into the
// "+LINE[:COL] path" form understood by emacs and emacsclient.
// Other arguments are returned unchanged.
std::vector<std::wstring> expandPositional(const std::wstring& arg);

// True if the (raw) argument string from the config already selects
// a frame mode for emacsclient (-c, -r, -t, -nw, -e, ...).
bool selectsFrameMode(std::wstring_view rawArgs);

}  // namespace rho

#endif  // RHO_ARGS_H
