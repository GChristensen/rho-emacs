
#include "select.h"
#include "resource.h"

#include <tchar.h>

TCHAR target[MAX_PATH];
size_t target_len = 0;

TCHAR target_path[MAX_PATH];
size_t target_path_len = 0;

HDC hMemDC = NULL;
BLENDFUNCTION bfn = {0};

const TCHAR *SWITCHES[] = 
{
	_T(""), // IDC_EMACS
	_T("/TARGET:EXT_SHELL"), // IDC_COMMAND_SHELL
	_T("/CLOZURECL"), // IDC_CCL
	_T("/SBCL"), // IDC_SBCL
	_T("/CLISP"), // IDC_CLISP
	_T("/ABCL"), // IDC_ABCL
	_T("/ECL"), // IDC_ECL
	_T("/Clojure"), // IDC_CLOJURE
	_T("/ClojureProject"), // IDC_CLOJURE_PROJECT
	_T("/MZSCHEME") // IDC_RACKET
};

void CenterWindow(HWND hwnd)
{
	RECT rc;
    
	GetWindowRect (hwnd, &rc);
    
	SetWindowPos(hwnd, 0, 
		(GetSystemMetrics(SM_CXSCREEN) - rc.right)/2,
		(GetSystemMetrics(SM_CYSCREEN) - rc.bottom)/2,
		0, 0, SWP_NOZORDER | SWP_NOSIZE);
}

void CheckComponent(HWND hwndDlg, const TCHAR *p_version_file, DWORD component)
{
	TCHAR version_file[MAX_PATH];

	_tcscpy(version_file, target_path);
	_tcscpy(version_file + target_path_len, p_version_file);
	
	DWORD attrs = GetFileAttributes(version_file);

//	if (attrs == INVALID_FILE_ATTRIBUTES)
//	{
		EnableWindow(GetDlgItem(hwndDlg, component), FALSE);

//	}
}


void CheckInstalledComponents(HWND hwndDlg)
{
	//CheckComponent(hwndDlg, _T("version\\emacs_version.txt"), IDC_EMACS);
	//CheckComponent(hwndDlg, _T("version\\emacs_version.txt"), IDC_COMMAND_SHELL);
	CheckComponent(hwndDlg, _T("version\\ClozureCL_version.txt"), IDC_CCL);
	CheckComponent(hwndDlg, _T("version\\SBCL_version.txt"), IDC_SBCL);
	CheckComponent(hwndDlg, _T("version\\CLISP_version.txt"), IDC_CLISP);
	CheckComponent(hwndDlg, _T("version\\ABCL_version.txt"), IDC_ABCL);
	CheckComponent(hwndDlg, _T("version\\ECL_version.txt"), IDC_ECL);
	CheckComponent(hwndDlg, _T("site\\lisp\\slime-legacy\\ChangeLog"), IDC_CLOJURE);
	CheckComponent(hwndDlg, _T("version\\Clojure_version.txt"), IDC_CLOJURE_PROJECT);
	CheckComponent(hwndDlg, _T("version\\MzScheme_version.txt"), IDC_RACKET);
}

INT_PTR CALLBACK DialogProc(HWND hwndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg)
	{
	case WM_INITDIALOG:
	{
		CenterWindow(hwndDlg);
		CheckInstalledComponents(hwndDlg);

	   	HANDLE hLogo = LoadBitmap(GetModuleHandle(NULL), 
									MAKEINTRESOURCE(IDB_LOGO));
   	
		HDC hdlgDC = GetDC(hwndDlg);
		hMemDC = CreateCompatibleDC(hdlgDC);
		ReleaseDC(hwndDlg, hdlgDC);

		SelectObject(hMemDC, (HBITMAP)hLogo);

		bfn.BlendOp = AC_SRC_OVER;
		bfn.BlendFlags = 0;
		bfn.SourceConstantAlpha = 255;
		bfn.AlphaFormat = AC_SRC_ALPHA;
	}
	return TRUE;
	case WM_COMMAND:
		switch (HIWORD(wParam))
		{
		case BN_CLICKED:
		{
			WORD id = LOWORD(wParam) - 1000;
			if (id >= 0 && id < sizeof(SWITCHES) / sizeof(SWITCHES[0]))
			{
				LaunchTarget(::target, SWITCHES[id]);
			}
			PostQuitMessage(0);
			return TRUE;
		}
        break;
        }
		break;
	case WM_CLOSE: 
        PostQuitMessage(0);
		return TRUE;
    case WM_PAINT:
	{
		HDC hdc;
		PAINTSTRUCT ps;

	    hdc = BeginPaint(hwndDlg, &ps);
	    AlphaBlend(hdc, 8, 4, 150, 292, hMemDC, 0, 0, 150, 292, bfn); 
	
    	EndPaint(hwndDlg, &ps);
	}
    break;
	}

	return FALSE;
}
          
INT ShowSelectDialog(HINSTANCE hInstance, TCHAR *target)
{
	_tcscpy(::target, target);
	target_len = _tcslen(::target);

	_tcscpy(target_path, target);
	TCHAR *bs = _tcsrchr(target_path, _T('\\'));

	if (bs)
	{
		*(bs + 1) = 0;
		target_path_len = _tcslen(target_path);
	}

	return DialogBox(hInstance, MAKEINTRESOURCE(IDD_SELECT), NULL, DialogProc);
} 
