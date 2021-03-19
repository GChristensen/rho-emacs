#ifndef RHO_STRUTILS_H
#define RHO_STRUTILS_H

#include <string>
#include <iostream>
#include <windows.h>

#ifdef UNICODE
#   define tstring std::wstring
#   define tcout std::wcout
#else
#   define tstring std::string
#   define tcout std::cout;
#endif

template <typename TChar, typename TStringGetterFunc>
std::basic_string<TChar> GetStringFromWindowsApi( TStringGetterFunc stringGetter, int initialSize = 0 )
{
    if( initialSize <= 0 )
    {
        initialSize = MAX_PATH;
    }

    std::basic_string<TChar> result( initialSize, 0 );
    for(;;)
    {
        auto length = stringGetter( &result[0], result.length() );
        if( length == 0 )
        {
            return std::basic_string<TChar>();
        }

        if( length < result.length() - 1 )
        {
            result.resize( length );
            result.shrink_to_fit();
            return result;
        }

        result.resize( result.length() * 2 );
    }
}

#endif //RHO_STRUTILS_H
