#include "gis.h"
#include "Vect.h"

static struct Map_info map;
static struct line_pnts *points;
static FILE *attfd;
static struct Categories cats;
static int first;
static char *vectfile;


static double
dmin(a,b)
    double a,b;
{
    return a < b ? a : b;
}

static double
dmax(a,b)
    double a,b;
{
    return a > b ? a : b;
}

openvect(name, title)
    char *name;
    char *title;
{

    vectfile=name;
    if (!Vect_open_new (&map, vectfile)) {
      fprintf (stderr, "WARNING: can't open vector map\n");
      exit(1);
    }
    points = Vect_new_line_struct();
    if ((attfd = G_fopen_new ("dig_att", vectfile)) == NULL) {
      fprintf (stderr, "WARNING: can't open vector file\n");
      exit(1);
    }
    G_init_cats ((CELL)0,title,&cats);
    first = 1;
}

writeline(x,y,n,xmid,ymid,cat,label)
    double *x, *y;
    double xmid, ymid;
    CELL cat;
    char *label;
    int n;
{
    int i;
    int type;
    char ttype;

    if (n>2) {
      type = AREA;
      ttype = 'A';
    }
    else {
      type = LINE;
      ttype = 'L';
    }

    if (first) {
	map.head.E = x[0];
	map.head.N = y[0];
	map.head.W = x[0];
	map.head.S = y[0];
	first = 0;
      }
    for (i=0; i<n; i++) {
	map.head.E = dmax(x[i],map.head.E);
	map.head.N = dmax(y[i],map.head.N);
	map.head.W = dmin(x[i],map.head.W);
	map.head.S = dmin(y[i],map.head.S);
      }

    Vect_copy_xy_to_pnts (points, x, y, n);
    Vect_write_line (&map, type, points);
    write_att (attfd, ttype, xmid, ymid, cat);

    G_set_cat (cat, label, &cats);
}

closevect()
{
    char command[1024];
    fclose (attfd);
    Vect_close (&map);
    G_write_vector_cats (vectfile, &cats);

/* build topology. -r will fill out header with a good region */
    sprintf (command, "v.support -r map='%s' option=build", vectfile);
    system(command);
}
