/* %W% %G% */

#include <stdio.h>

static char *explanation[] = {
"if=file of=cellfile rows=# cols=# bpc=#[u]\\",
"      latres=# lonres=# xx=lat,lon s=spheroid",
"where",
"  if      = input file",
"  of      = resultant cell file",
"  rows    = number of rows in input file",
"  cols    = number of columns in input file",
"  bpc     = number of bytes per column in input file",
"            follow number with 'u' if data is unsigned",
"  latres  = resolution of latitude in seconds",
"  lonres  = resolution of longitude in seconds",
"  xx      = the latitude,longitude of one corner of the input file",
"            eg. to specify the southwest corner: sw=46.0.0n,120.0.0w",
"  s       = spheroid of local datum",
0};

usage(me) char *me;
{
    char **s;
    fprintf (stderr, "usage: %s ", me);

    for (s = explanation; *s; s++)
	fprintf (stderr, "%s\n", *s);
}
