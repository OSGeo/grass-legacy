/*  %W%  %G%  */

#define COLOR   1
#define SIZE    2
#define TYPE    3
#define INFILE  4

#define TYPE_X  1
#define TYPE_PLUS   2
#define TYPE_BOX    3
#define TYPE_DIAMOND    4

#ifdef MAIN
struct variables {
    char *alias;
    int position;
}   variables[] = {

    "size", SIZE,
    "s", SIZE,
    "color", COLOR,
    "c", COLOR,
    "type", TYPE,
    "t", TYPE,
    "file", INFILE,
    "f", INFILE
};
static int n_variables = 8;

char color[64];
int size;
int type;
FILE *infile;

#else
extern char color[64];
extern int size;
extern int type;
extern FILE *infile;

#endif
