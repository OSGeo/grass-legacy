
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/List.h>

extern Window display_win;
extern char cell_displayed[25];
extern int list_cell_flag;

void draw_map(draw, client_data, call_data)
	Widget draw;
	caddr_t client_data;
	caddr_t call_data;
{	
	char buf[100];
	XtListReturnStruct *map;

	
	XtUnmapWidget(XtParent(draw));	
	list_cell_flag = 0;	
	map = (XtListReturnStruct *) call_data;
		
	
	
	
	
	sprintf(cell_displayed, "%s", map->string);
	printf("\n Map drawn");
	printf("\n displ_win = %d", display_win);
	sprintf(buf, "Xcell %u %s", 
			display_win, cell_displayed);
	system(buf);

}
