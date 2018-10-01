#ifndef  _H_SELECT__
#define _H_SELECT__

#define WINVER 0x0500

#include <windows.h>

DWORD LaunchTarget(const TCHAR *target, const TCHAR *arguments, bool show = false, bool wait = false);

INT ShowSelectDialog(HINSTANCE hInstance, TCHAR *target);
 
#endif
