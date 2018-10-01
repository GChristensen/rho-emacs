// Class for CommandLineToArgvW stack scope memory management
// (C) 2010 g/christensen

#include "CmdLine.h"

CmdLine::CmdLine(TCHAR *cmdline): m_argc(0)
{
    m_trueargs = m_args = CommandLineToArgvW(cmdline, &m_argc);

#if defined(__GNUC__) && defined(_UNICODE) || defined(NOCRT)
    m_args += 1;
    m_argc -= 1;
#endif
}

CmdLine::~CmdLine()
{
    LocalFree(m_trueargs);
}

int CmdLine::getArgc()
{
    return m_argc;
}

TCHAR **CmdLine::getArgv()
{
    return m_args;
}
