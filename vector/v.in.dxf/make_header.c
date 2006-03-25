#include <stdlib.h>
#include "global.h"

/* JUST FOR REFERENCE */
/*  struct dig_head
 * char organization[30] ;
 * char date[20] ;
 * char your_name[20] ;
 * char map_name[41] ;
 * char source_date[11] ;
 * long  orig_scale ;
 * char line_3[73] ;
 * int plani_zone ;
 * double W, E, S, N ;
 * double digit_thresh ;
 * double map_thresh ;
 */

int make_header(DXF_DIG * Layer)
{
    char *date;
    char *name;
    char *organization;

    /* print binary file using the dig_head structure and the write_head_binary func */

    /* CALCULATE TODAY'S DATE */

    date = G_date();
    /* DETERMINE USER'S NAME */
    name = G_whoami();


    if (getenv("GRASS_ORGANIZATION")) {	/* added MN 5/2001 */
	organization = (char *)getenv("GRASS_ORGANIZATION");
	Vect_set_organization(&head, organization);
    }
    else
	Vect_set_organization(&head, "GRASS Development Team");
    Vect_set_date(&head, date);
    Vect_set_person(&head, name);
    Vect_set_map_name(&head, dxf_file);
    head.head.source_date = (char *)G_malloc(1);
    head.head.source_date[0] = 0;
    head.head.line_3 = (char *)G_malloc(1);
    head.head.line_3[0] = 0;
    /*
     * strcpy(head.source_date, );  MAP DATE
     * strcpy(head.line_3, ); OTHER INFO
     */
    head.head.orig_scale = 2400;
    head.head.plani_zone = 0;
    head.plus.box.W = 0;
    head.plus.box.E = 0;
    head.plus.box.S = 0;
    head.plus.box.N = 0;
    head.head.digit_thresh = 0.;

    /*
     * head.head.map_thresh = 0.0;
     */
    Vect_copy_head_data(&head, Layer->Map);

    return 0;
}
