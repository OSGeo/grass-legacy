

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>


extern Widget top_level;

void quit(qt, client_data, call_data)
Widget qt;
caddr_t client_data;
caddr_t call_data;
{

    XtDestroyWidget((Widget) top_level);

    exit(1);
}
