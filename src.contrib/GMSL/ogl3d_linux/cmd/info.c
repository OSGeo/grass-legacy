
/* inform:
** lets the user know that the program is 
** working fine.
*/


#include "interface.h"
static int Inform_ok = 0;

int 
init_inform (void)
{
    Inform_ok = 1;
}


void 
inform (data_cell *dc, char *str)
{
  XmString str2;
  Arg wargs[3];
  int n;
    
    if(XtIsRealized(dc->toplevel)){
        str2 = XmStringCreateSimple(str);
        n = 0;
        XtSetArg(wargs[n], XmNlabelString, str2); n++;
        XtSetValues(dc->status,wargs,n); 
        XmUpdateDisplay(dc->toplevel);

        XmStringFree(str2);
    }
    else{
	fprintf(stderr,"%s\n", str);
    }

}
  

  

