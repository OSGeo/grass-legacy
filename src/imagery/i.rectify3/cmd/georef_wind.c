/*======================================================================
Filename: georef.c
Moudle:   i.rectify3  (cmd)
Author:   Mike Baba


     get_window (struct Ref ref, tRect_Data *rect_data)

        Three scenerios bases upon the command options:
	(1) If the -c flag is set use the current window in the
	    target location.
	(2) Else if the -m flag is set calculate and use the minimal
	    georeferenced window.
	(3) Otherwise the user must have set the target region parameters
	    (north, south, ...) on the command line.  If so use them.


Modifications:
30 Oct 93    - mbaba       - original 


======================================================================*/

#include "global.h"
#include "parse.h"
#include "protodefs.h"

int 
get_window (struct Ref ref, tRect_Data *rect_data)
{
    char   msg[80];
    struct Cell_head cellhd;



    /* is the -c flag set */
    if (rect_data->current == 1) {
       return 0;
    }


    /* is the -m flag set */
    if (rect_data->minimal == 1) {

      /* get the source image header */
      if (G_get_cellhd (ref.file[ref_list[0]].name, 
			ref.file[ref_list[0]].mapset,
			&cellhd) < 0) {
	sprintf (msg, "Can't open header for source image %s\n", 
		 ref.file[ref_list[0]].name);
	G_fatal_error (msg);
      }

      /* determine the minimal window */
      georef_window (&cellhd, &target_window);
      return 0;
    }

    /* set the target window from the command line parameters */
    check_window(rect_data);

    return 0;
}



int georef_window (struct Cell_head *w1, struct Cell_head *w2)
{
    double e,  n;          /* east, north, elev in target */
    double nused1, nused2;     /* for values not-used */


    /* Calculate the transformation parameters */
    group.calculate_trans(&group);


    /* transform each of the corners of the source imagery */
    /* north-west */
    group.forward_trans (&group, w1->west, w1->north, nused1,
			 &e, &n, &nused2);
    w2->north = w2->south = n;
    w2->west  = w2->east  = e;


    /* north-east */
    group.forward_trans (&group, w1->east, w1->north, nused1,
			 &e, &n, &nused2);
    if (n > w2->north) w2->north = n;
    if (n < w2->south) w2->south = n;
    if (e > w2->east ) w2->east  = e;
    if (e < w2->west ) w2->west  = e;


    /* south-west */
    group.forward_trans (&group, w1->west, w1->south, nused1,
			 &e, &n, &nused2);
    if (n > w2->north) w2->north = n;
    if (n < w2->south) w2->south = n;
    if (e > w2->east ) w2->east  = e;
    if (e < w2->west ) w2->west  = e;


    /* south-east */
    group.forward_trans (&group, w1->east, w1->south, nused1,
			 &e, &n, &nused2);
    if (n > w2->north) w2->north = n;
    if (n < w2->south) w2->south = n;
    if (e > w2->east ) w2->east  = e;
    if (e < w2->west ) w2->west  = e;
    

    /* determin the resolution */
    w2->ns_res = (w2->north - w2->south) / w1->rows;
    w2->ew_res = (w2->east  - w2->west ) / w1->cols;

    return 0;
}

