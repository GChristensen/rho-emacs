// Some handy string utilites
// (C) 2010 g/christensen

#include <windows.h>
#include "StrUtils.h"

void StrShiftR(TCHAR *str, int shift_to)
{
	int n = 0;
	for (; str[n] != 0; ++n);

	for (TCHAR *point = &str[n]; point >= str; --point)
		*(point + shift_to) = *point;
}

void StrShiftL(TCHAR *str, int shift_to)
{
	TCHAR *point = str;
	for (; *point != 0; ++point)
		*(point - shift_to) = *point;

	*(point - shift_to) = 0;
}

// Useful to quote `"' and `\' in strings being inserted into elisp expressions
int QuoteChar(TCHAR *str, TCHAR ch, int max_len)
{
	int n = 0;
	int chars = 0;

	for (; str[n] != 0; ++n)
	{
		if (str[n] == ch)
		{
			++chars;
		}
	}

	if (n + chars + 1 > max_len)
		return 0;

	for (TCHAR *point = str + n; point >= str; --point)
	{
		if (*point == ch)
		{
			StrShiftR(point, 1);
			*point = _T('\\');
		}
	}

	return n + chars;
}

// Replaces placeholder argument `%1' to first element in argv if supplied
// (currently only one argument placeholder `%1' is supported)
int InterpolateArguments(TCHAR* str, int max_len, TCHAR **argv, int argc)
{
	TCHAR *point = _tcschr(str, _T('%'));

	int str_len = lstrlen(str);

	if (!point)
		return str_len;

	if (argc > 0)
	{
		TCHAR *arg_to_interpolate = new TCHAR[max_len];
		lstrcpy(arg_to_interpolate, argv[0]);

		int quoted_len = 0;

		if (_tcsstr(str, _T("--eval")))
			quoted_len = QuoteChar(arg_to_interpolate, _T('\\'), max_len);
		else 
			quoted_len = _tcslen(arg_to_interpolate);

		if (!quoted_len || str_len + quoted_len + 1 > max_len)
		{
			delete arg_to_interpolate;
			return 0;
		}

		StrShiftR(point, quoted_len - 2);
		_tcsncpy(point, arg_to_interpolate, quoted_len);
		delete arg_to_interpolate;

		return str_len + quoted_len;
	}
	else
	{
		StrShiftL(point + 2, 2);
		return str_len - 2;
	}
}

int RTrim(TCHAR *str)
{
    int i;
    int len;
    int retval = 0;

    len = _tcslen(str);

    for
    (
      i= len - 1;
      i >= 0 && (str[i] == ' ' || str[i] == '\t' || str[i] == '\r'
                 || str[i] == '\n');
      i--
    )
    {
        str[i] = (TCHAR)0;
        retval++;
    }

    return retval;
}