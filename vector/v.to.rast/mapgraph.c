#include "gis.h"
#include "Vect.h"
#include "local.h"

static char *colors[] =
{
    "red", "green", "blue", "white", "yellow", "orange", "purple",
    NULL
};
static int xc = 0;

static FILE *fd = NULL;
int 
begin_mapgraph (void)
{
    FILE *popen();
    fd = NULL;
    if (G__getenv("MONITOR"))
    {
	if( (fd = popen ("d.mapgraph","w")) )
	    fprintf (fd, "color white\n");
    }
    return fd != NULL;
}
int
mapgraph_line (int x1, int y1, int x2, int y2)
{
    double east, north;

    if (fd == NULL) return 0;
    G_plot_where_en (x1, y1, &east, &north);
    fprintf (fd, "move %lf %lf\n", east,north);
    G_plot_where_en (x2, y2, &east, &north);
    fprintf (fd, "draw %lf %lf\n", east,north);
    fflush (fd);
    return 0;
}
int
mapgraph_polygon (double *x, double *y, int n)
{
    return ( mapgraph(x,y,n,1) );
}
int 
mapgraph_polyline (double *x, double *y, int n)
{
    return ( mapgraph(x,y,n,0)) ;
}
int 
mapgraph (double *x, double *y, int n, int polygon)
{
    if (fd == NULL) return 0;
    if (polygon)
    {
	if (!colors[xc])xc = 0;
	fprintf (fd, "color %s\n", colors[xc++]);
    }

    fprintf (fd, polygon?"polygon\n":"polyline");
    while (n-- > 0)
	fprintf (fd, " %lf %lf\n", *x++, *y++);
    fflush (fd);
    return 0;
}
int 
end_mapgraph (void)
{
    if (fd == NULL) return 0;
    pclose (fd);
    fd = NULL;
    return 0;
}
