#include "xgen.h"

char *
FindFixedFont(dpy)
    Display *dpy;
{
    char **list;
    XFontStruct *info;
    int count;
    int i;

    list = XListFontsWithInfo(dpy,"*",1024,&count,&info);
    for ( i = 0; i < count; i++ ) {
        if ( IsFixedFont(info[i]) ) return(list[i]);
    }
    return(NULL);
}


IsFixedFont(fs)
    XFontStruct fs;
{
    return(fs.min_bounds.width == fs.max_bounds.width);
}
