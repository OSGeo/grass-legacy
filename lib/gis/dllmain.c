#include <windows.h>
#include <stdlib.h>
#include <fcntl.h>

BOOL WINAPI DllMain(HINSTANCE hInst, DWORD fdwReason, LPVOID lpvReserved)
{
	_fmode = O_BINARY;
	return TRUE;
}
