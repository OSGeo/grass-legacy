/****************************************************************/
/*								*/
/*	write_area.c	in	~/src/libes/dlg			*/
/*								*/
/*	#include "dlg.h"					*/
/*								*/
/*	dlg_write_area (fd,dlg,area)				*/
/*      FILE *fd              file containing dlg header info	*/
/*      struct dlg *dlg       structures containing dlg info	*/
/*      int area              area for which coors.to be written*/
/*								*/
/*	returns:  -1 on error					*/
/*                 0 on completion				*/
/*								*/
/*	This routine takes the coordinate info about an area	*/ 
/*	from the dlg structure and writes it into a dlg-3 file	*/
/*								*/
/****************************************************************/

#include "dlg.h"
#include <stdio.h>

dlg_write_area (fd,dlg,area)
    FILE *fd ;
        struct dlg *dlg ;
	int area;
{
        if (fseek(fd, dlg->area_off[area], 0) != 0)
                return(-1) ;
        _dlg_write_area(&dlg->area, fd) ;
        return(0) ;
}

/************* END OF FUNCTION "DLG_WRITE_AREA"	*****************/

