/*  %W%  %G%  */
/* Command argument positions */
#define SPHEROID    1
#define MODE    2

#define SILENT  0
#define LOUD    1

#ifdef MAIN
struct variables {
    char *alias;
    int position;
}   variables[] = {

    "s", SPHEROID,
    "spheroid", SPHEROID,
    "m", MODE,
    "mode", MODE
};
static int n_variables = 4;

int have_spheroid;
int mode;

#else
extern int have_spheroid;
extern int mode;

#endif
