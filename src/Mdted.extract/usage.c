/* %W% %G% */
#include "dma.h"

static char *explanation[] = {
"if=tapedev of=file hf=file n=lat s=lat e=lon w=lon",
"where",
"  if  = tape device (default /dev/rmt0)",
"  of  = output file",
"  hf  = header file describing output file",
"  n   = north latitude of window to extract ( format: dd.mm.ss[n|s] )",
"  s   = south latitude of window to extract ( format: dd.mm.ss[n|s] )",
"  e   = east longitude of window to extract ( format: dd.mm.ss[e|w] )",
"  w   = west longitude of window to extract ( format: dd.mm.ss[e|w] )",
0};

usage()
{
    char **s;
    fprintf (stderr, "usage: %s ", PGM);

    for (s = explanation; *s; s++)
	fprintf (stderr, "%s\n", *s);
}
