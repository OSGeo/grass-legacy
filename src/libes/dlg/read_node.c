/****************************************************************/
/*								*/
/*	read_node.c 	in	~/src/libes/dlg			*/
/*								*/
/*	#include "dlg.h"					*/
/*								*/
/*      dlg_read_node (fd,dlg,node)				*/
/*      FILE *fd              file containing dlg header info	*/
/*      struct dlg *dlg       structures containing dlg info	*/
/*      int node              node for which coors. desired	*/
/*								*/
/*      returns:  -1 on error					*/
/*                 0 on completion				*/
/*								*/
/*	This routine reads information about a node from a	*/
/*	dlg-3 file and stores it in the dlg structure		*/
/*								*/
/****************************************************************/

#include "dlg.h"
#include <stdio.h>

dlg_read_node (fd,dlg,node)
    FILE *fd ;
        struct dlg *dlg ;
    int node ;
{
#ifdef FOO
/*DEBUG*/ fprintf (stderr, "In dlg_read_node [%d]\n", node);
#endif
        if (fseek(fd, dlg->node_off[node], 0) != 0)
                return(-1) ;
        _dlg_read_node(&dlg->node, fd) ;
        return(0) ;
}

/****************** END OF FUNCTION "DLG_READ_NODE" *************/
