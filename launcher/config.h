#ifndef RHO_CONFIG_H
#define RHO_CONFIG_H

#include <optional>
#include <string>
#include <string_view>

#include "winutil.h"

namespace rho {

// Thin wrapper over the rho.conf / rhoc.conf INI file.
class Config {
public:
    explicit Config(fs::path file) : file_(std::move(file)) {}

    const fs::path& file() const { return file_; }

    // Returns std::nullopt if the key is absent (an empty value is a value).
    std::optional<std::wstring> get(std::wstring_view section, std::wstring_view key) const;

    // Tries the keys in order and returns the first one present.
    std::optional<std::wstring> getAny(std::wstring_view section,
                                       std::initializer_list<std::wstring_view> keys) const;

private:
    fs::path file_;
};

// Resolves a configured path: absolute paths are kept, relative ones
// are taken relative to `base`.
fs::path resolvePath(const fs::path& base, std::wstring_view value);

// Documents\rho-emacs of the current user.
Result<fs::path> defaultHome();

}  // namespace rho

#endif  // RHO_CONFIG_H
