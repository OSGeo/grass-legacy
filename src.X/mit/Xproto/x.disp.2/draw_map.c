
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/List.h>

extern char cell_displayed[25];
extern int list_cell_flag;

void draw_map(draw, closure, calldata)
Widget draw;
caddr_t closure, calldata;
{
    char buf[100];
    XawListReturnStruct *map;

    XtUnmapWidget(XtParent(draw));
    list_cell_flag = 0;
    map = (XawListReturnStruct*)calldata;

    sprintf(cell_displayed, "%s", map->string);
    sprintf(buf, "Xcell %s", cell_displayed);
    system(buf);
    fprintf(stderr, "Map \"%s\" drawn\n", cell_displayed);
}
