#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
extern char cell_displayed[25];
extern Window display_win;
extern int n_vects;
extern char *vector_files[5];


void erase_display(erase, client_data, call_data)
	Widget erase;
	caddr_t client_data;
	caddr_t call_data;
{	
	register k;	
	XClearWindow(XtDisplay(erase), display_win);

	cell_displayed[0] = '\0';

	for(k = 0; k < n_vects; k++)
	{
	vector_files[k] = NULL;
	}

	n_vects = 0;




}
