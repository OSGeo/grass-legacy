
/*
**  Written by Dave Gerdes  Summer 1990
**  US Army Construction Engineering Research Lab
*/

#include "gis.h"
#include "externs.h"

do_vect_display ()
{
    register int i, cnt;
    double vect[3];

    Vect_rewind (&Map);

    vect[2] = Vect_z;	/* Z is fixed */

    frontbuffer (1);
    backbuffer (0);

    do_fast_vect_display ();

    cpack (0xffffff);	/* white */

    while (1)
    {
	if (0 > Vect_read_next_line (&Map, Points))
	    break;

	cnt = 0;
	bgnline ();
	for (i = 0 ; i < Points->n_points ; i++, cnt++)
	{
	    vect[0] = Points->x[i] - wind.west;
	    vect[1] = Points->y[i] - wind.south;
	    v3d (vect);
	    if (cnt > 253)
	    {
		endline ();
		bgnline ();
		cnt = 0;
	    }
	}
	endline ();
    }
    frontbuffer (0);
    backbuffer (1);
}

do_fast_vect_display ()
{
    register int i, cnt;
    double *x, *y;
    double vect[3];
    int notdone = 1;
    int n_points;
    int ret;

    vect[2] = Vect_z;	/* Z is fixed */
    cpack (0x777777);	/* white */

    bgnclosedline ();
    vect[0] = X_Min;
    vect[1] = Y_Min;
    v3d (vect);
    vect[0] = X_Min;
    vect[1] = Y_Max;
    v3d (vect);
    vect[0] = X_Max;
    vect[1] = Y_Max;
    v3d (vect);
    vect[0] = X_Max;
    vect[1] = Y_Min;
    v3d (vect);
    endclosedline ();
    /*do_test_vect_display ();*/
}

do_test_vect_display ()
{
    register int i, cnt;
    double *x, *y;
    double vect[3];
    int notdone = 1;
    int n_points;
    int ret;

    vect[2] = Z_Max_real;	/* Z is fixed */
    cpack (0x0000ff);	/* red */

    bgnclosedline ();
    vect[0] = X_Min;
    vect[1] = Y_Min;
    v3d (vect);
    vect[0] = X_Min;
    vect[1] = Y_Max;
    v3d (vect);
    vect[0] = X_Max;
    vect[1] = Y_Max;
    v3d (vect);
    vect[0] = X_Max;
    vect[1] = Y_Min;
    v3d (vect);
    endclosedline ();
}
