/*****************************************************************************/
/***                                                                       ***/
/***                             open_files()                              ***/
/***   	                    Opens NTF input file.  			   ***/
/***               Jo Wood, Project ASSIST, 19th May 1993                  ***/
/***                                                                       ***/
/*****************************************************************************/

#include "ntf_in.h"


open_files()
{
    /* Open existing file and set the input file descriptor. */

    if ( (ntf_fptr=fopen(ntf_in_name,"r")) == NULL)
    {
        char err[256];
        sprintf(err,"Problem opening NTF file.");
        G_fatal_error(err);
    }

    line_number= 0;		/* Initialise line counter	*/

}
