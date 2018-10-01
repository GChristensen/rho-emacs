// Class for CommandLineToArgvW stack scope memory management
// (C) 2010 g/christensen

#ifndef _H_CMDLINE
#define _H_CMDLINE

#include <tchar.h>
#include <windows.h>

class CmdLine
{
private:
	TCHAR **m_trueargs;
	TCHAR **m_args;
	int m_argc;

public:
	CmdLine(TCHAR *cmdline);
	~CmdLine();

	int getArgc();
	TCHAR **getArgv();
};

#endif // _H_CMDLINE
