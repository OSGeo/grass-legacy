/*****************************************************************************/
/***                                                                       ***/
/***                              open_vect()                              ***/
/***         Opens sites file and a new vector file for output.  	   ***/
/***            Jo Wood, Project ASSIST, 19th July, 1995                   ***/
/***                                                                       ***/
/*****************************************************************************/

#include "delaunay.h"


int open_files(Map)
    struct Map_info 	*Map;		/* Structure to hold vector information	*/

{
    /*--------------------------------------------------------------------------*/
    /*                              INITIALISE					*/
    /*--------------------------------------------------------------------------*/ 

    struct dig_head	Vect_head;	/* Structure holding vector header info	*/
    struct Cell_head    region;         /* Structure to hold region information */

    int			status;		/* Level of file to open (1) or 	*/
					/* negative on failure.			*/

    char		text[255];	/* Used to hold a line of text.		*/



    G_get_window(&region);              /* Fill out the region structure	*/


    /*--------------------------------------------------------------------------*/
    /*                       OPEN EXISITING SITES FILE				*/
    /*--------------------------------------------------------------------------*/ 
 
    if ((sites_fptr=G_fopen_sites_old(sites_in_name,mapset_in)) == NULL)
    {
        char err[256];
        sprintf(err,"Problem opening sites file <%s>",sites_in_name);
        G_fatal_error(err);
    }

    /*--------------------------------------------------------------------------*/
    /*                          OPEN NEW VECTOR FILE				*/
    /*--------------------------------------------------------------------------*/ 

    status = Vect_open_new(Map,vect_out_name);  	/* New vector map.	*/


    /*--------------------------------------------------------------------------*/
    /*                    WRITE OUT VECTOR HEADER INFORMATION			*/
    /*--------------------------------------------------------------------------*/ 

    sprintf(text,"Dept. of Geography");
    strcpy(Vect_head.organization,text);

    strcpy(Vect_head.date,G_date());

    sprintf(text,"created by s.delaunay");
    strcpy(Vect_head.your_name,text);

    sprintf(text,"Delaunay triangulation of %s",sites_in_name);
    strcpy(Vect_head.map_name,text);

    Vect_head.source_date[0]	= 0;
    Vect_head.orig_scale	= 0;
    Vect_head.line_3[0] 	= 0;
    Vect_head.plani_zone	= region.zone;
    Vect_head.digit_thresh	= 0;
    Vect_head.map_thresh	= 0;


    Vect_head.N	   = region.north;
    Vect_head.S	   = region.south;
    Vect_head.E	   = region.east;
    Vect_head.W	   = region.west;

    Vect_copy_head_data(&Vect_head,&(Map->head));/* This line MUST be included	*/
						/* as it tells GRASS that the	*/
						/* header has been updated.	*/


    /*--------------------------------------------------------------------------*/
    /*            RETURN POINTER TO STRUCTURE HOLDING MAP INFORMATION		*/
    /*--------------------------------------------------------------------------*/ 

    return(status);

}
