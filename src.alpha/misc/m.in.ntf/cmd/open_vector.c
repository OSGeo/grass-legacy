/*****************************************************************************/
/***                                                                       ***/
/***                             open_vector()                             ***/
/***   	                  Opens  GRASS vector files.  			   ***/
/***                 Jo Wood, Project ASSIST, 30th May 1993.               ***/
/***                                                                       ***/
/*****************************************************************************/

#include "ntf_in.h"

open_vector(extension)
    char	*extension;		/* Filename extension.			*/
{
    /*--------------------------------------------------------------------------*/
    /*                              INITIALISE                                  */
    /*--------------------------------------------------------------------------*/ 

    struct dig_head  vect_head;      	/* Structure holding vector header info */
    struct Cell_head region;         	/* Structure to hold region information */


    char             text[255],      	/* Used to hold a line of text.         */
		     vect_out_name[80]; /* Name of vector to open.		*/

    strcpy(vect_out_name,file_out_name);
    strcat(vect_out_name,extension);	/* Append extension to default file name*/

    G_get_window(&region);


    /*--------------------------------------------------------------------------*/
    /*                          OPEN NEW VECTOR FILE                            */
    /*--------------------------------------------------------------------------*/ 

    mapset_out = G_mapset();            /* Current mapset.			*/

    if (G_legal_filename(vect_out_name)==NULL)
    {
        char err[256];
        sprintf(err,"Illegal vector file name. Please try another.");
        G_fatal_error(err);
    }
    else
    {
        if (G_find_vector2(vect_out_name,mapset_out) !=NULL)
        {
            char err[256];
            sprintf(err,"Vector map [%s] exists.\nPlease try another.\n",vect_out_name);
            G_fatal_error(err);
        }
    }

    if (Vect_open_new(&vect_info,vect_out_name) < 0)
    {
    	char err[256];
    	sprintf(err,"Unable to open vector map [%s].\n",vect_out_name);
    	G_fatal_error(err);
    }

    if ( (vect_info.att_fp = G_fopen_new("dig_att",vect_out_name)) <0)
    {
    	char err[256];
    	sprintf(err,"Unable to open vector attribute map [%s].\n",vect_out_name);
    	G_fatal_error(err);
    }

    O_vector = TRUE;

    /*--------------------------------------------------------------------------*/
    /*                    WRITE OUT VECTOR HEADER INFORMATION                   */
    /*--------------------------------------------------------------------------*/ 

    strcpy(vect_head.organization,H_organ);

    strcpy(vect_head.date,H_ddate);

    sprintf(text,"created by m.in.ntf");
    strcpy(vect_head.your_name,text);

    strcpy(vect_head.map_name,H_mname);

    strcpy(vect_head.source_date,H_mdate);

    vect_head.orig_scale        = H_scale;
    vect_head.line_3[0]         = 0;
    vect_head.plani_zone        = region.zone;
    vect_head.digit_thresh      = 0;
    vect_head.map_thresh        = 0;

    vect_head.N    = (double)Y_max;      
    vect_head.S    = (double)Y_min;
    vect_head.E    = (double)X_max;
    vect_head.W    = (double)X_min;


    Vect_copy_head_data(&vect_head,&(vect_info.head));
                                                /* This line MUST be included   */
                                                /* as it tells GRASS that the   */
                                                /* header has been updated.     */

}
