#include "xincludes.h"
#include <stdio.h>

static Widget text;
static Widget info, info2;

set_text_widget (w)
    Widget w;
{
    text = w;
}

void
showtext (w, data, call_data)
    Widget w;
    char   *data;
    caddr_t *call_data;
{
    XmTextSetString (text, data);
    XtVaSetValues (text, XmNcursorPosition, 0, NULL);
}

set_info_widget (w)
    Widget w;
{
    info = w;
}

set_info2 (w)
    Widget w;
{
    info2 = w;
}

void
write_info(n, data)
    int n;
    char   *data;
{
    Widget w;

    w = (n == 1)? info : info2;
    
    XmTextSetString (w, data);
    XtVaSetValues (w, XmNcursorPosition, 0, NULL);
}

void
add_info(n, data)
    int n;
    char   *data;
{
    Widget w;
    char *str, buf[1000];

    w = (n == 1)? info : info2;
    str = XmTextGetString (w);
    sprintf (buf, "%s\n%s", str, data);
    XmTextSetString (w, buf);
    free (str);
}
