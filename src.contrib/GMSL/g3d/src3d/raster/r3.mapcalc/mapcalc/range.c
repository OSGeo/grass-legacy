#include "glob.h"

int
print_range (result)
    char *result;
{
    void *fd;
    double dmin, dmax;
    char *mapset;

    mapset = G_mapset();

    if((fd = G3d_openCellOld (result, mapset,&current_region,G3D_TILE_SAME_AS_FILE,G3D_USE_CACHE_DEFAULT)) == NULL)
    {
      	fprintf (stderr, "\nOOPS can't open cell file for print range[%s]\n", result);
       	return 0;
    }

    if (!G3d_range_load (fd))
    {
      	fprintf (stderr, "\nOOPS can't can't load range from [%s]\n", result);
       	return 0;
    }
    G3d_range_min_max (fd, &dmin, &dmax);

    if (ISNULL_D(&dmin) || ISNULL_D(&dmax))
	return 0;

    printf ("range: %.10lf %.10lf\n", dmin, dmax);

    if (! G3d_closeCell (fd))
    {
        fprintf (stderr, "\nOOPS can't close cell file for print range[%s]\n", result);
        return 0;
    }
}