/*  ./src/libes/dlg/read_line.c
 *******************************************************************
 *  #include "dlg.h"
 *
 *  dlg_read_line (fd,dlg,line)
 *      FILE *fd              file containing dlg header info
 *      struct dlg *dlg       structures containing dlg information
 *      int line              line for which coors. desired
 *
 * returns:  -1 on error
 *            0 on completion
 *            1 if no line to load
 */

#include "dlg.h"
#include <stdio.h>

dlg_read_line (fd,dlg,line)
    FILE *fd ;
	struct dlg *dlg ;
    int line ;
{
	if (dlg->line_off[line] == NULL)
	    return (1);
	if (fseek(fd, dlg->line_off[line], 0) != 0)
		return(-1) ;
	_dlg_read_line(&dlg->line, fd) ;
	return(0) ;
}
