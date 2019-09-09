// Some handy string utilites (the app is aimed to not use CRT explicitly,
// but currently no CRT detachment is made)
// (C) 2010 g/christensen

#ifndef  _H_STR_UTILS_
#define _H_STR_UTILS_

#include <tchar.h>


void StrShiftR(TCHAR *str, int shift_to);
void StrShiftL(TCHAR *str, int shift_to);
int InterpolateArguments(TCHAR* str, int max_len, TCHAR **argv, int argc);
int RTrim(TCHAR *str);
TCHAR *StrReplace(TCHAR *orig, TCHAR *rep, TCHAR *with);

#endif // _H_STR_UTILS_
