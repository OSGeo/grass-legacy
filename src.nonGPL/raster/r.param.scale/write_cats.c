/*****************************************************************************/
/***                                                                       ***/
/***                            write_cats()                               ***/
/***   	        Writes out category file for morphometric features	   ***/
/***               Jo Wood, Project ASSIST, 7th February 1995              ***/
/***                                                                       ***/
/*****************************************************************************/

#include "param.h"

write_cats()
{
    /*------------------------------------------------------------------------*/
    /*                            INITIALISE                                  */
    /*------------------------------------------------------------------------*/ 

    struct Categories	cats;

    G_init_cats(NUM_CATS,"Surface Features",&cats);

    /*------------------------------------------------------------------------*/
    /*                      FILL OUT CATEGORIES STRUCTURE                     */
    /*------------------------------------------------------------------------*/ 

    
    G_set_cat(PIT,    " Pit",          &cats);
    G_set_cat(PEAK,   " Peak",         &cats);
    G_set_cat(RIDGE,  " Ridge",        &cats);
    G_set_cat(CHANNEL," Channel",      &cats);
    G_set_cat(PASS,   " Pass (saddle)",&cats);
    G_set_cat(FLAT,   " Planar",       &cats);


    /*------------------------------------------------------------------------*/
    /*                     WRITE OUT CATEGORIES STRUCTURE                     */
    /*------------------------------------------------------------------------*/ 

    if (G_write_cats(rast_out_name,&cats) <=0)
    {
	char warn[255];
	sprintf(warn,"Can't write category file for <%s>",rast_out_name);
	G_warning(warn);
    }

    G_free_cats(&cats);

}
