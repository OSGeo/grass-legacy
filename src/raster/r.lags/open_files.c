/*****************************************************************************/
/***                                                                       ***/
/***                             open_files()                              ***/
/***   	              Opens input and output raster files.  		   ***/
/***               Jo Wood, Project ASSIST, 24th January 1993              ***/
/***                                                                       ***/
/*****************************************************************************/

#include "lags.h"


open_files()
{
    char err[256];	/* Sting holding error message.		*/


    /* Open existing file and set the input file descriptor. */
    /* ----------------------------------------------------- */

    if ( (fd_in=G_open_cell_old(rast_in_name,mapset_in)) <0)
    {
        sprintf(err,"Problem opening input file [%s].",rast_in_name);
        G_fatal_error(err);
    }


    /* Open new files and set the output file descriptor. */
    /* -------------------------------------------------- */

    if (measure == MORAN) 
    {
    	if ( (fd1_out=G_open_cell_new(rast_out1_name,mapset_out)) <0)
    	{
            sprintf(err,"Problem opening output file [%s].",rast_out1_name);
            G_fatal_error(err);
    	}
/*
	if ( (fd2_out=G_open_cell_new(rast_out2_name,mapset_out)) <0)
    	{
            sprintf(err,"Problem opening output file [%s].",rast_out2_name);
            G_fatal_error(err);
    	}
*/
    }

    if (measure == TEXTURAL)
    {
        strcpy(con_name,rast_out1_name);
        strcat(con_name,".con");

        if ( (fd_con = G_open_cell_new(con_name,mapset_out)) <0)
    	{
            sprintf(err,"Problem opening output file [%s].",con_name);
            G_fatal_error(err);
    	}


        strcpy(asmo_name,rast_out1_name);
        strcat(asmo_name,".asmo");

        if ( (fd_asmo = G_open_cell_new(asmo_name,mapset_out)) <0)
    	{
            sprintf(err,"Problem opening output file [%s].",asmo_name);
            G_fatal_error(err);
    	}


        strcpy(ent_name,rast_out1_name);
        strcat(ent_name,".ent");

        if ( (fd_ent = G_open_cell_new(ent_name,mapset_out)) <0)
    	{
            sprintf(err,"Problem opening output file [%s].",ent_name);
            G_fatal_error(err);
    	}


        strcpy(asym_name,rast_out1_name);
        strcat(asym_name,".asym");

        if ( (fd_asym = G_open_cell_new(asym_name,mapset_out)) <0)
    	{
            sprintf(err,"Problem opening output file [%s].",asym_name);
            G_fatal_error(err);
    	}


        strcpy(idm_name,rast_out1_name);
        strcat(idm_name,".idm");

        if ( (fd_idm = G_open_cell_new(idm_name,mapset_out)) <0)
    	{
            sprintf(err,"Problem opening output file [%s].",idm_name);
            G_fatal_error(err);
    	}
    }
}
