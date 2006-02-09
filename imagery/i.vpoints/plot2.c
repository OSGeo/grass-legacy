/* plot2() - Level One vector reading */

#include <unistd.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/display.h>
#include "vectpoints.h"
#include "globals.h"

int plot2 (char *name, char *mapset, struct line_pnts *Points)
{
    double *x, *y;
    int i, np;
    int nlines;
    struct Cell_head window;
    struct Map_info P_map;
    char msg[100];

     /*
    sprintf (msg,"Initializing [%s] ... ", name);
     debug(msg);
     */

    /*
    if (NULL != (err = dig__P_init (name, mapset, &P_map)))
    */

    if (2 > Vect_open_old (&P_map, name, mapset))
    {
	/*
	fprintf(stderr, "\nWARNING: vector file [%s] - Could not open Level 2\n", name);
	*/
	return -1;
     }

     sprintf(msg,"W = %f E = %f\n",P_map.head.W,P_map.head.E);
     debug(msg);
     sprintf(msg,"S = %f N = %f\n",P_map.head.S,P_map.head.N);
     debug(msg);

     sprintf(msg,"proj = %d regio= %d\n",P_map.proj,P_map.Constraint_region_flag);
     debug(msg);

     sprintf(msg,"open = %X mode = %d level = %d\n",P_map.open,P_map.mode,
      P_map.level);
     debug(msg);
     sprintf(msg,"nlines = %d nnodes = %d\n",P_map.n_lines,P_map.n_nodes);
     debug(msg);

     sprintf(msg,"next_line = %d\n",P_map.next_line);
     debug(msg);

     sprintf(msg,"constraint_type = %d constraint_type_flag = %d\n",
     P_map.Constraint_type, P_map.Constraint_type_flag);
     debug(msg);

     /*
     fprintf (stderr, "Plotting ... "); fflush (stdout);
     */

     G_get_set_window (&window);
     /*
     dsp_setup(0);
     */
     sprintf(msg, "D_get_d_north = %f\n",D_get_d_north() );
     debug(msg);
     sprintf(msg, "D_get_d_south = %f\n",D_get_d_south() );
     debug(msg);
     sprintf(msg, "D_get_d_east = %f\n",D_get_d_east() );
     debug(msg);
     sprintf(msg, "D_get_d_west = %f\n",D_get_d_west() );
     debug(msg);

     G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);

     nlines = V2_num_lines (&P_map);

     /*
      sprintf(msg,"nlines = %ld\n",nlines);
     debug(msg);
     */

#ifdef OLD
    for (line = 1; line <= nlines; line++)
    {
	if (V2_get_line_bbox (&P_map, line, &N, &S, &E, &W) < 0)
	{
	fprintf (stderr, "\nWARNING: vector file [%s] - read error\n", name);

	return -1;
     }
     if (!G_window_overlap (&window, N, S, E, W)) 
	continue;
     if (V2_read_line (&P_map, Points,line) < 0)
     {
	fprintf (stderr, "\nWARNING: vector file [%s] - read error\n", name);

	return -1;
     }
       
	np = Points->n_points;
	x = Points->x;
	y = Points->y;
	for(i=1; i < np; i++)
	{
	    G_plot_line(x[0], y[0], x[1], y[1]);
	    x++;
	    y++;
	}
     }
#endif

     /* let library do window checking for us */
     Vect_set_constraint_region (&P_map, window.north, window.south, 
    window.east, window.west);
     /*
     sprintf(msg,"s = %f n = %f\n",window.south,window.north);
     debug(msg);
     sprintf(msg,"w = %f e = %f\n",window.west,window.east);
     debug(msg);
     */

     i = Vect_read_next_line(&P_map, Points);
     sprintf(msg,"ret = %d\n",i);
     debug(msg);
     if(i < 1)
	return 0;

	np = Points->n_points;
	x = Points->x;
	y = Points->y;
     sprintf(msg,"np = %d x = %f y = %f\n",np,x[0],y[0]);
     debug(msg);

    while (1)
    {
	int ret;

	if (0 > (ret = Vect_read_next_line (&P_map, Points)))
	{
	    if (ret == -1)
		G_warning ("Read error\n");
	    break;
        }
	np = Points->n_points;
	x = Points->x;
	y = Points->y;
	for(i=1; i < np; i++)
	{
	    G_plot_line(x[0], y[0], x[1], y[1]);
	    x++;
	    y++;
	}
    }
/*
    fprintf (stderr, "Done\n");
*/
    return 0;
}
