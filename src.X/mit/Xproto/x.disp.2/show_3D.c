#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

extern char cell_displayed[25];

void show_3D(show, client_data, call_data)
Widget show;
caddr_t client_data;
caddr_t call_data;
{
    char buf[100];

    fprintf(stderr, "\n 3D View");
    sprintf(buf, "x3d %s", cell_displayed);
    system(buf);
}
