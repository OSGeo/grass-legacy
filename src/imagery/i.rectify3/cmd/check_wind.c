#include "global.h"
#include "parse.h"

static int round (double *);

int check_window (tRect_Data *rect_data)
{
    double south, west;
    double x;
    char   msg[80];

    /* switch to target env */
    select_target_env();


    /* north= */
    if (rect_data->north) {
      if (G_scan_northing (rect_data->north, &x, target_window.proj))
	target_window.north = x;
      else {
	sprintf (msg, "<north=%s> ** illegal value **\n",
		 rect_data->north);
	G_fatal_error (msg);
      }
    }

    /* south= */
    if (rect_data->south) {
      if (G_scan_northing (rect_data->south, &x, target_window.proj))
	target_window.south = x;
      else {
	sprintf (msg, "<south=%s> ** illegal value **\n",
		 rect_data->south);
	G_fatal_error (msg);
      }
    }

    /* east= */
    if (rect_data->east) {
      if (G_scan_easting (rect_data->east, &x, target_window.proj))
	target_window.east = x;
      else {
	sprintf (msg, "<east=%s> ** illegal value **\n",
		 rect_data->east);
	G_fatal_error (msg);
      }
    }

    /* west= */
    if (rect_data->west) {
      if (G_scan_easting (rect_data->west, &x, target_window.proj))
	target_window.west = x;
      else {
	sprintf (msg, "<west=%s> ** illegal value **\n",
		 rect_data->west);
	G_fatal_error (msg);
      }
    }


    /* res= */
    if (rect_data->res) {
      if (!G_scan_resolution (rect_data->res, &x, target_window.proj)) {
	sprintf (msg, "<res=%s> ** illegal value **\n",
		 rect_data->res);
	G_fatal_error (msg);
      }
      target_window.ns_res = x;
      target_window.ew_res = x;
    }
     
    /* nsres= */
    if (rect_data->nsres) {
      if (!G_scan_resolution (rect_data->nsres, &x, target_window.proj)) {
	sprintf (msg, "<nsres=%s> ** illegal value **\n",
		 rect_data->nsres);
	G_fatal_error (msg);
      }
      target_window.ns_res = x;
    }

    /* ewres= */
    if (rect_data->ewres) {
      if (!G_scan_resolution (rect_data->ewres, &x, target_window.proj)) {
	sprintf (msg, "<ewres=%s> ** illegal value **\n",
		 rect_data->ewres);
	G_fatal_error (msg);
      }
      target_window.ew_res = x;
    }


    target_window.rows  = 0 ;
    target_window.cols  = 0 ;

    round (&target_window.north);
    round (&target_window.south);
    round (&target_window.east);
    round (&target_window.west);
    round (&target_window.ew_res);
    round (&target_window.ns_res);

    if ( (target_window.ns_res <= 0 ) || (target_window.ew_res <= 0) )
    {
        sprintf(msg, "Illegal resolution value(s)\n") ;
	G_fatal_error (msg);
    }
    if (target_window.north <= target_window.south)
    {
        sprintf (msg, "North must be larger than south\n");
	G_fatal_error (msg);
    }
    if (target_window.east <= target_window.west)
    {
	sprintf (msg, "East must be larger than west\n");
	G_fatal_error (msg);
    }



    /* if the north-south is not multiple of the resolution,
     *    round the south downward
     */
    south = target_window.south;
    target_window.rows = (target_window.north - target_window.south + target_window.ns_res/2)
			/ target_window.ns_res;
    target_window.south = target_window.north - target_window.rows * target_window.ns_res;

    /* do the same for the west */
    west = target_window.west ;
    target_window.cols = (target_window.east - target_window.west + target_window.ew_res/2)
			/ target_window.ew_res;
    target_window.west = target_window.east - target_window.cols * target_window.ew_res;



    /* switch back to current env */
    select_current_env();

    return 0;
}


static int round (double *x)
{
    char xs[40];
    sprintf (xs, "%.2f", *x);
    sscanf (xs, "%lf", x);

    return 0;
}
