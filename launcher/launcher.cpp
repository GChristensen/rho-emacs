// LispCabinet Launcher 0.3
// (C) 2010 g/christensen

// A simple application for command-line emacslicent silent launch,
// (so we get rid of nasty GUI emacsclientw window).
// Can use predefined command-line templates from a configuration file.

#ifndef TERMINAL
#   include "select.h"
#endif
#include "resource.h"
#include "StrUtils.h"
#include "CmdLine.h"

#include <tchar.h>
#include <shlobj.h>

#define MAX_ARGS_LENGTH 2047
#define MAX_CMDLINE_LENGTH 32768

#define GENERAL_SECTION _T("GENERAL")
#define ARGUMENTS_SECTION _T("ARGUMENTS")

void create_dir(TCHAR* path)
{
	TCHAR dir_name[MAX_PATH];
	TCHAR* p = path;
	TCHAR* q = dir_name;

	while (*p)
	{
		if ((_T('\\') == *p) || (_T('/') == *p))
		{
			if (':' != *(p - 1))
			{
				CreateDirectory(dir_name, NULL);
			}
		}

		*q++ = *p++;
		*q = '\0';
	}

	CreateDirectory(dir_name, NULL);
}

DWORD LaunchTarget(const TCHAR *target, const TCHAR *arguments, bool show,
		bool wait)
{
    TCHAR home_dir[MAX_PATH];
    ZeroMemory(home_dir, MAX_PATH * sizeof(home_dir[0]));
    GetEnvironmentVariable(_T("HOME"), home_dir, MAX_PATH);

	TCHAR server_file[MAX_PATH];
    ZeroMemory(server_file, MAX_PATH * sizeof(server_file[0]));

	_tcscpy(server_file, home_dir);
	_tcscpy(server_file + _tcslen(server_file), _T("\\.emacs.d"));

    DWORD attrs = GetFileAttributes(server_file);

	if (attrs == INVALID_FILE_ATTRIBUTES)
	{
		create_dir(server_file);
	}

    _tcscpy(server_file + _tcslen(server_file), _T("\\server"));

    attrs = GetFileAttributes(server_file);

	if (attrs == INVALID_FILE_ATTRIBUTES)
	{
		create_dir(server_file);
	}

    _tcscpy(server_file + _tcslen(server_file), _T("\\server"));

    SetEnvironmentVariable(_T("EMACS_SERVER_FILE"), server_file);


	STARTUPINFOW siStartupInfo;
	PROCESS_INFORMATION piProcessInfo;
	ZeroMemory(&siStartupInfo, sizeof(siStartupInfo));
	ZeroMemory(&piProcessInfo, sizeof(piProcessInfo));
	siStartupInfo.cb = sizeof(siStartupInfo);

	bool batch_file = false;

	if (_tcsstr(target, _T(".bat")))
		batch_file = true;

    size_t target_len = _tcslen(target);

	TCHAR args[target_len + _tcslen(arguments) + 5];
	ZeroMemory(args, sizeof(args));

	// Prepend target as a first argument
	
	args[0] = _T('"');
	_tcscpy(args + 1, target);
	args[target_len + 1] = _T('"');
	args[target_len + 2] = _T(' ');

	if (arguments)
		_tcscpy(args + target_len + 3, arguments);
//				MAX_ARGS_LENGTH - target_len - 4);

	if (CreateProcess(batch_file ? NULL : target, args, NULL, NULL, TRUE,
			(show ? 0 : CREATE_NO_WINDOW), NULL, NULL, &siStartupInfo,
			&piProcessInfo))
	{
		if (wait)
			WaitForSingleObject(piProcessInfo.hProcess, INFINITE);

        CloseHandle(piProcessInfo.hThread);
		CloseHandle(piProcessInfo.hProcess);
		return 0;
	}

	return 3;
}

// Add the path of the private JRE installation directory to Emacs `PATH' 
// env. variable. (it could not be done inside Emacs)
void portable_jre_env_hack(const TCHAR *module_name)
{
	// get PATH env. var
	DWORD new_path_len = GetEnvironmentVariable(_T("PATH"), NULL, 0);
	new_path_len += MAX_PATH;

	TCHAR *path = new TCHAR[new_path_len];

	GetEnvironmentVariable(_T("PATH"), path, new_path_len);
	DWORD path_len = _tcslen(path);

	*(path + path_len) = _T(';');
	path_len += 1;

	TCHAR *point = _tcsrchr(module_name, _T('\\'));
	size_t prefix_len = point - module_name + 1;

	_tcsncpy(path + path_len, module_name, prefix_len);
	path_len += prefix_len;

	_tcscpy(path + path_len, _T("bin\\jdk\\bin"));

	SetEnvironmentVariable(_T("PATH"), path);

	delete[] path;
}

void get_default_home(TCHAR *home_dir, const TCHAR *module_name)
{
	SHGetFolderPath(NULL, CSIDL_PERSONAL, NULL, 0, home_dir);
    _tcscpy(home_dir + _tcslen(home_dir), _T("\\rhome"));
}

TCHAR *set_home_variable(HINSTANCE hInstance)
{
	TCHAR home_dir[MAX_PATH] = { 0 };
	TCHAR module_name[MAX_PATH];
	TCHAR module_dir[MAX_PATH];
	TCHAR sandbox_file[MAX_PATH];

    ZeroMemory(home_dir, MAX_PATH * sizeof(home_dir[0]));

//	GetEnvironmentVariable(_T("HOME"), home_dir, MAX_PATH);
	GetModuleFileName(hInstance, module_name, MAX_PATH);
	_tcscpy(module_dir, module_name);
	TCHAR *p = _tcsrchr(module_dir, _T('\\'));
	*p = 0;

	_tcscpy(sandbox_file, module_dir);
	_tcscpy(sandbox_file + _tcslen(sandbox_file), _T("\\sandbox"));

	DWORD attrs = GetFileAttributes(sandbox_file);

	if (attrs == INVALID_FILE_ATTRIBUTES)
	{
		get_default_home(home_dir, module_name);
	}
	else
	{
		HANDLE hFile;
		DWORD dwRead;
		char line[MAX_PATH];

        ZeroMemory(line, MAX_PATH * sizeof(line[0]));
 
		hFile = CreateFile(sandbox_file, GENERIC_READ, FILE_SHARE_READ, NULL,
				OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
		ReadFile(hFile, line, MAX_PATH, &dwRead, NULL);
		CloseHandle(hFile);

		mbstowcs(home_dir, line, MAX_PATH);

		RTrim(home_dir);

		if (home_dir[0] == 0)
		{
			get_default_home(home_dir, module_name);
		}
		else if (home_dir[0] == _T('.') && home_dir[1] == _T('.'))
		{
			TCHAR *p = _tcsrchr(module_dir, _T('\\'));
			_tcscpy(p + 1, home_dir + 3);
			_tcscpy(home_dir, module_dir);
		}
	}


	SetEnvironmentVariable(_T("HOME"), home_dir);

    return _tcsdup(home_dir);
}

void del_desktop_lock_hack(TCHAR *home_dir)
{
	TCHAR lock_file[MAX_PATH];

	_tcscpy(lock_file, home_dir);

	_tcscpy(lock_file + _tcslen(lock_file), _T("\\.emacs.d\\.emacs.desktop.lock"));

	DeleteFile(lock_file);
}

bool switch_presents(TCHAR **argv, int argc, TCHAR *switch_)
{
	for (int i = 0; i < argc; ++i)
	{
		if (!_tcsicmp(argv[i], switch_))
			return true;
	}

	return false;
}

int entry(HINSTANCE hInstance, int argc, TCHAR **argv) {
    int arg_offset = 0;

	// EXE name
	TCHAR module_name[MAX_PATH];
	GetModuleFileName(hInstance, module_name, MAX_PATH);

	size_t module_name_len = lstrlen(module_name);

	TCHAR module_dir[MAX_PATH];
	_tcscpy(module_dir, module_name);
	TCHAR *slash = _tcsrchr(module_dir, _T('\\'));
	*slash = NULL;
	SetEnvironmentVariable(_T("RHO_DIR"), module_dir);

    TCHAR emacs_dir[MAX_PATH];
	_tcscpy(emacs_dir, module_name);
	slash = _tcsrchr(emacs_dir, _T('\\'));
    _tcscpy(slash, _T("\\bin\\emacs"));
	SetEnvironmentVariable(_T("EMACS_DIR"), emacs_dir);

	// Path to the launch target
	TCHAR target_path[MAX_PATH] =
	{	0};
	TCHAR target_key[MAX_PATH] = _T("TARGET");

	// Config path
	size_t base_name_len = module_name_len - 2;
	TCHAR config_name[MAX_PATH];
	lstrcpyn(config_name, module_name, base_name_len);

	TCHAR *point = config_name + base_name_len - 1;
	lstrcpy(point, _T("conf"));

	// Check if a config file exists
	DWORD attrs = GetFileAttributes(config_name);

	// If the first argument begins with "/TARGET:", use it as a target key in [ARGUMENTS]
	if (argc > 0 && !_tcsncmp(argv[0], _T("/TARGET:"), 8))
	{
		_tcscpy(target_key, argv[0] + 1);
	}

	GetPrivateProfileString(GENERAL_SECTION, target_key, NULL, target_path,
			MAX_PATH, config_name);

	if (!target_path[0])
	{
		return 2;
	}

	if (!_tcschr(target_path, _T(':')))
	{ // Relative path
		TCHAR *point = _tcsrchr(module_name, _T('\\'));
		size_t prefix_len = point - module_name + 1;

		// Prepend folder of the executable to target path
		StrShiftR(target_path, prefix_len);
		_tcsncpy(target_path, module_name, prefix_len);
	}

	TCHAR select_flag[10];

	GetPrivateProfileString(GENERAL_SECTION, _T("SELECT"), NULL, select_flag,
			sizeof(select_flag) / sizeof(TCHAR), config_name);

    portable_jre_env_hack(module_name);

	if (select_flag[0] == _T('t'))
	{
#ifndef terminal
		return ShowSelectDialog(hInstance, target_path);
#endif
	}

	TCHAR *home_dir = set_home_variable(hInstance);        

	// Command line arguments which launcher should pass to the target
	// (depend on command line switch)
	TCHAR args_to_pass[MAX_ARGS_LENGTH] = {0};

	if (argc > 0)
	{
		// Check if first argument is a switch
		if (argv[arg_offset][0] == _T('/'))
		{
            if (_tcsstr(argv[arg_offset], _T("/HOME:")) != NULL)
            {
                SetEnvironmentVariable(_T("HOME"), argv[arg_offset] + 6);
                if (switch_presents(argv + arg_offset + 1, argc - arg_offset - 1, _T("/GUEST")))
                    SetEnvironmentVariable(_T("ANYHOME"), _T("t"));

                return LaunchTarget(target_path, args_to_pass);
            }
            else
            {
                SetEnvironmentVariable(_T("INVOCATION_TAG"), argv[arg_offset] + 1);

                bool org_protocol = switch_presents(argv, argc, _T("/TARGET:ORG_PROTOCOL"));

                TCHAR args_to_pass[org_protocol? MAX_CMDLINE_LENGTH: MAX_ARGS_LENGTH] = {0};

                GetPrivateProfileString(ARGUMENTS_SECTION, argv[arg_offset] + 1, NULL, args_to_pass,
                                        org_protocol? MAX_CMDLINE_LENGTH: MAX_ARGS_LENGTH, config_name);

                if (args_to_pass[0])
                {
                    bool show = switch_presents(argv + arg_offset + 1, argc - arg_offset - 1, _T("/SHOW"));
                    bool wait = switch_presents(argv + arg_offset + 1, argc - arg_offset - 1, _T("/WAIT"));

                    // delete the .desktop.lock file in home directory if presents
                    if (switch_presents(argv, argc, _T("/TARGET:PRECOMP"))
                        || switch_presents(argv, argc, _T("/TARGET:EXT_SHELL"))) {

                        del_desktop_lock_hack(home_dir);
                    }

                    InterpolateArguments(args_to_pass, org_protocol? MAX_CMDLINE_LENGTH: MAX_ARGS_LENGTH, argv + arg_offset + 1, argc - 1);

                    if (org_protocol) {                 
                        TCHAR *correct_args = StrReplace(args_to_pass, _T("/?"), _T("?"));
                        _tcscpy(args_to_pass, correct_args);
                        free(correct_args);
                    }

                    return LaunchTarget(target_path, args_to_pass, show, wait);
                }
                else
                {
                    arg_offset += 1;
                }
            }
		}
	}

	int n_args = argc - arg_offset;
	TCHAR arg1[MAX_PATH];
	TCHAR *argv2[1] = {arg1};

	// make the file path passed through argument absolute
	if (n_args > 0)
	{
		int arg_len = _tcslen(argv[arg_offset]);

		if ((arg_len > 1 && argv[arg_offset][1] != _T(':')) || arg_len == 1)
		{
			GetCurrentDirectory(MAX_PATH, arg1);
			arg_len = _tcslen(arg1);
			arg1[arg_len++] = _T('\\');
			_tcscpy(arg1 + arg_len, argv[arg_offset]);
		}
		else
		_tcscpy(arg1, argv[arg_offset]);
	}

	// Read fallback args 
	GetPrivateProfileString(ARGUMENTS_SECTION, _T(":FALLBACK"), NULL, args_to_pass,
			MAX_ARGS_LENGTH, config_name);

	InterpolateArguments(args_to_pass, MAX_ARGS_LENGTH, argv2, n_args);
	return LaunchTarget(target_path, args_to_pass);

}

int WINAPI _tWinMain(HINSTANCE hInstance,
		HINSTANCE hPrevInstance,
		LPTSTR lpCmdLine,
		int nCmdShow)
{
	CmdLine cmd_line(lpCmdLine);
	int argc = cmd_line.getArgc();
	TCHAR **argv = cmd_line.getArgv();

    return entry(hInstance, argc, argv);
}

//#if defined(__GNUC__) && defined(_UNICODE) && !defined(_tWinMain)
int WinMain(HINSTANCE hInstance,
		HINSTANCE hPrevInstance,
		LPSTR lpCmdLine,
		int nCmdShow)
{ 
	return _tWinMain(hInstance, NULL, GetCommandLine(), 0);
}
//#endif

/*int main(int argc, TCHAR **argv) {
    return entry(GetModuleHandle(NULL), argc, argv);
}*/
