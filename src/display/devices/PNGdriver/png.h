#include <gd.h>

#ifndef BUFSIZ
#include <stdio.h>
#endif

#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

#define DEF_WIDTH  640
#define DEF_HEIGHT 480
#define FILE_NAME  "map.png"

GLOBAL char *file_name;
GLOBAL FILE *output;
GLOBAL gdImagePtr im;
GLOBAL int currentColor;

/* connect.c */
int get_connection(char *, int *, int *);
int prepare_connection(void);
int check_connection(char *, char *);
