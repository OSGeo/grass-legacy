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
#define FILE_NAME  "htmlmap"

#define INITIAL_TEXT 1000

#define APACHE 0     /* write output in apache/ncsa server image map format */
#define NCSA   0     /* write output in apache/ncsa server image map format */
#define CLIENT 1     /* write output in netscape client side image map format */
#define RAW    2     /* write output in raw format */


GLOBAL char *last_text;
GLOBAL int   last_text_len;
GLOBAL char *file_name;
GLOBAL int   html_type;
GLOBAL FILE *output;

struct MapPoly {
    char *url;
    int  num_pts;
    int  *x_pts;
    int  *y_pts;
    struct MapPoly *next_poly;
} MapPoly;

GLOBAL struct MapPoly *head;
GLOBAL struct MapPoly **tail;

