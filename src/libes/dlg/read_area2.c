/****************************************************************/
/*								*/
/*	read_area2.c	in	~/src/libes/dlg			*/
/*								*/
/*	#include "dlg.h"					*/
/*								*/ 
/*	dlg_read_area (fd,dlg,area)				*/
/*      FILE *fd              file containing dlg header info	*/
/*      struct dlg *dlg       structures containing dlg info	*/
/*      int area              area for which coors. desired	*/
/*								*/
/*	returns:  -1 on error					*/
/*                 0 on completion				*/
/*                 1 no area to load				*/
/*								*/
/*	This routine reads info about an area from the dlg-3	*/
/*	file into the dlg structure.				*/
/*								*/
/****************************************************************/

#include "dlg.h"
#include <stdio.h>

dlg_read_area (fd,dlg,area)
    FILE *fd ;
        struct dlg *dlg ;
    int area ;
{
	if (dlg->area_off[area] == NULL)
	    return (1);

        if (fseek(fd, dlg->area_off[area], 0) != 0)
                return(-1) ;
        _dlg_read_area(&dlg->area, fd) ;
        return(0) ;
}

/*************** END OF FUNCTION "DLG_READ_AREA" ****************/

