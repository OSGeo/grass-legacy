/*  %W%  %G%  */

#include "driver.h"

#define TEXTCOLOR   1
#define BACKCOLOR   2
#define SIZE    3
#define FONT    4

#ifdef MAIN
struct variables {
    char *alias;
    int position;
}   variables[] = {

    "textcolor", TEXTCOLOR,
    "tc", TEXTCOLOR,
    "backcolor", BACKCOLOR,
    "bc", BACKCOLOR,
    "size", SIZE,
    "s", SIZE,
    "font", FONT,
    "f", FONT
};
static int n_variables = 8;

char textcolor[32];
char backcolor[32];
float size;
int dots_per_line;

#else
extern char textcolor[32];
extern char backcolor[32];
extern float size;
extern int dots_per_line;

#endif
