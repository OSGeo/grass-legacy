
/* before_pops:
** determines which constant to send the pops() routine
*/

#include "interface.h"


void 
before_pops (Widget w, data_cell *dc, caddr_t ignored)
{
	XmString *str, str2;
	Arg wargs[3];
	int n;


	n = 0;
	XtSetArg(wargs[n], XmNlabelString, &str); n++;
	XtGetValues(w, wargs, n); /* gets label & puts it at str */

	str2 = XmStringCreateSimple(" New Color File... ");	
	if(XmStringCompare(str,str2)){
		pops(w,dc, COLOR_FILE);
		return;
	}

	str2 = XmStringCreateSimple(" New Elevation File... ");	
	if(XmStringCompare(str,str2)){
		pops(w,dc, ELEVATION_FILE);
		return;
	}

	str2 = XmStringCreateSimple(" New Vector File... ");	
	if(XmStringCompare(str,str2)){
		pops(w,dc, VECTOR_FILE);
		return;
	}

	str2 = XmStringCreateSimple(" New Sites File... ");	
	if(XmStringCompare(str,str2)){
		pops(w,dc, SITES_FILE);
		return;
	}

}


