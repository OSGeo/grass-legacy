#include <stdio.h>

usage(me)
{
    char buf[300];
	char buf2[100];

/*    sprintf (buf, "usage: %s project=file soils=file cover=file\n", me); */
    sprintf (buf, "%s -w div=file elev=file soils=file cover=file stream=file", me);
/*	strcat(buf,buf2); */
    G_fatal_error (buf);
    exit(1);
}
