/****************************************************************/
/*								*/
/*	write_line.c	in 	~/src/libes/dlg			*/
/*								*/
/*	#include "dlg.h"					*/
/*								*/
/*	dlg_write_line (fd,dlg,line)				*/
/*      FILE *fd              file containing dlg header info	*/
/*      struct dlg *dlg       structures containing dlg info	*/
/*      int line              line whose coors.are to be written*/
/*								*/
/*	returns:  -1 on error					*/
/*                 0 on completion				*/
/*								*/
/*	This routine takes the info about a line from the 	*/
/*	dlg structure and writes it into a dlg-3 file		*/
/*								*/
/****************************************************************/

#include "dlg.h"
#include <stdio.h>

dlg_write_line (fd,dlg,line)
    FILE *fd ;
	struct dlg *dlg ;
	int line;
{
	if (fseek(fd, dlg->line_off[line], 0) != 0)
		return(-1) ;
	_dlg_write_line(&dlg->line, fd) ;
	return(0) ;
}

/************ END OF FUNCTION "DLG_WRITE_LINE" ******************/
