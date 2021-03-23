#include "strutils.h"


bool iequals(const tstring& a, const tstring& b)
{
    return std::equal(a.begin(), a.end(),
                      b.begin(), b.end(),
                      [](TCHAR a, TCHAR b) {
                          return tolower(a) == tolower(b);
                      });
}