#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

extern Display *dpy;
XFontStruct *font1, *font2, *font3, *font4, *font5;

create_fonts()
{
    font1 = XLoadQueryFont(dpy,
        "-adobe-times-bold-r-normal--18-180-75-75-p-99-iso8859-1");

    font2 = XLoadQueryFont(dpy,
        "-adobe-times-bold-r-normal--14-140-75-75-p-77-iso8859-1");

    font3 = XLoadQueryFont(dpy,
        "-adobe-times-bold-r-normal--12-120-75-75-p-67-iso8859-1");

    font4 = XLoadQueryFont(dpy,
        "-adobe-times-bold-r-normal--10-100-75-75-p-57-iso8859-1");

    font5 = XLoadQueryFont(dpy,
        "-adobe-times-medium-r-normal--12-120-75-75-p-64-iso8859-1");
}

