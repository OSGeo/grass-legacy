#include "xgen.h"


void 
ShellDestroyedCB(w,cld,cad)
	Widget w;
	caddr_t cld;
	caddr_t cad;
{
	Shell *shell = (Shell *)cld;
	
	shell->widget = NULL;
	shell->popup = False;
}


void 
ShellPopdownCB(w,cld,cad)
	Widget w;
	caddr_t cld;
	caddr_t cad;
{
	Shell *shell = (Shell *)cld;
	
	shell->popup = False;
}
