/*  %W%  %G%  */

#define SIZE    1
#define COLOR   2

#ifdef MAIN
struct variables {
    char *alias;
    int position;
}   variables[] = {

    "size", SIZE,
    "s", SIZE,
    "color", COLOR,
    "c", COLOR
};
static int n_variables = 4;

char color[64];
int size;

#else
extern char color[64];
extern int size;

#endif
