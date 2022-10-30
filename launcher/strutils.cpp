#include <algorithm> 
#include <cctype>
#include <locale>

#include "strutils.h"

bool iequals(const tstring& a, const tstring& b)
{
    return std::equal(a.begin(), a.end(),
                      b.begin(), b.end(),
                      [](TCHAR a, TCHAR b) {
                          return tolower(a) == tolower(b);
                      });
}

inline void ltrim(tstring &s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) {
        return !std::isspace(ch);
    }));
}

inline void rtrim(tstring &s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) {
        return !std::isspace(ch);
    }).base(), s.end());
}

inline void trim(tstring &s) {
    ltrim(s);
    rtrim(s);
}

inline tstring ltrim_copy(tstring s) {
    ltrim(s);
    return s;
}

inline tstring rtrim_copy(tstring s) {
    rtrim(s);
    return s;
}

tstring trim_copy(tstring s) {
    trim(s);
    return s;
}