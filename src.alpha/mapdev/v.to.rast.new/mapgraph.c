#include "gis.h"
static char *colors[] =
{
    "red", "green", "blue", "white", "yellow", "orange", "purple",
    NULL
};
static int xc = 0;

static FILE *fd = NULL;
begin_mapgraph()
{
    FILE *popen();
    fd = NULL;
    if (G__getenv("MONITOR"))
    {
	if(fd = popen ("d.mapgraph","w"))
	    fprintf (fd, "color white\n");
    }
    return fd != NULL;
}
mapgraph_line(x1,y1,x2,y2)
{
    double east, north;

    if (fd == NULL) return;
    G_plot_where_en (x1, y1, &east, &north);
    fprintf (fd, "move %lf %lf\n", east,north);
    G_plot_where_en (x2, y2, &east, &north);
    fprintf (fd, "draw %lf %lf\n", east,north);
    fflush (fd);
}
mapgraph_polygon(x,y,n)
    double *x, *y;
{
    mapgraph(x,y,n,1);
}
mapgraph_polyline(x,y,n)
    double *x, *y;
{
    mapgraph(x,y,n,0);
}
mapgraph(x,y,n,polygon)
    double *x, *y;
{
    if (fd == NULL) return;
    if (polygon)
    {
	if (!colors[xc])xc = 0;
	fprintf (fd, "color %s\n", colors[xc++]);
    }

    fprintf (fd, polygon?"polygon\n":"polyline");
    while (n-- > 0)
	fprintf (fd, " %lf %lf\n", *x++, *y++);
    fflush (fd);
}
end_mapgraph()
{
    if (fd == NULL) return;
    pclose (fd);
    fd = NULL;
}
