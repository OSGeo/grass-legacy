/* plot1() - Level One vector reading */

#include "gis.h"

extern double D_get_d_north();
extern double D_get_d_south();
extern double D_get_d_west();
extern double D_get_d_east();

extern int D_move_abs();
extern int D_cont_abs();


plot1 (name, mapset)
    char *name, *mapset;
{
    FILE *fd, *open_vect() ;
    double *x, *y;
    int i, np;

    fd = open_vect (name, mapset);

    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);


    printf ("Plotting ... "); fflush (stdout);
    while (1)
    {
        switch (dig_read_next_line (fd, &np, &x, &y))
	{
	case -1:
	    close_vect(fd);
	    fprintf (stderr, "\nERROR: vector file [%s in %s] - can't read\n",
		    name, mapset);
	    return -1;
	case -2: /* EOF */
	    printf ("Done\n");
	    close_vect(fd);
	    return  0;
	}

	for(i=1; i < np; i++)
	{
	    G_plot_line (x[0], y[0], x[1], y[1]);
	    x++;
	    y++;
	}
    }
}
