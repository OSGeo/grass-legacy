/****************************************************************/
/*								*/
/*	#include "dlg.h"					*/
/*								*/
/*	dlg_write_node (fd,dlg,node)				*/
/*      FILE *fd              file containing dlg header info	*/
/*      struct dlg *dlg       structures containing dlg info	*/
/*      int node              node for which coors.to be written*/
/*								*/
/*	returns:  -1 on error					*/
/*                 0 on completion				*/
/*								*/
/*	This routine takes info about a node from the dlg	*/
/*	structure and writes it to the dlg-3 file		*/
/*								*/
/****************************************************************/

#include "dlg.h"
#include <stdio.h>

dlg_write_node (fd,dlg,node)
    FILE *fd ;
        struct dlg *dlg ;
	int node;
{
        if (fseek(fd, dlg->node_off[node], 0) != 0)
                return(-1) ; 
        _dlg_write_node(&dlg->node, fd) ;
        return(0) ;
}

/************ END OF FUNCTION "DLG_WRITE_NODE" ******************/
