#include <stdio.h>

usage(me)
{
    char buf[300];
    char buf2[100];

    sprintf (buf, 
      "usage: %s -n div=inputlayer streams=inputlayer sim=simtitle\n", me);
/*
    sprintf (buf2, 
      "OR:\n       %s -o sim=simtitle", me);

    strcat(buf,buf2);
*/

    G_fatal_error (buf);
    exit(-1);
}
