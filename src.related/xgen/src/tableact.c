/**********************************************************************
   tableact.c - perform actions when a table is activated
 *********************************************************************/
/*******************************************************************************
Xgen was developed by Kurt Buehler, while at the Center for Advanced Decision
Support for Water and Environmental Systems (CADSWES), University of Colorado
at Boulder and at the Indiana Water Resources Research Center (IWRRC),
Purdue University for the U.S. Army Construction Engineering Research
Laboratory in support of the Geographical Resources Analysis Support
System (GRASS) software. The example scripts were developed by Ms. Christine
Poulsen of USA-CERL, much thanks goes to her for her work.

Permission to use, copy, modify and distribute without charge this software,
documentation, etc. is granted, provided that this comment is retained,
and that the names of Kurt Buehler, Christine Poulsen, CADSWES, IWRRC,
the University of Colorado at Boulder, Purdue University, or USA-CERL are not
used in advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

The author disclaims all warranties with regard to this software, including
all implied warranties of merchantability and fitness, in no event shall
the author be liable for any special, indirect or consequential damages or
any damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of this
software.
*******************************************************************************/
#include "xgen.h"


/*****************************************************************
 *  TableCB - the callback associated with all tables.
 ****************************************************************/

void
TableCB(w, cld, cad)
    Widget                          w;
    caddr_t                         cld;
    caddr_t                         cad;
{
    /***************************************************************
     * the client data field is a pointer to the table's object information
     **************************************************************/

    /***************************************************************
     * the call data field is a pointer to the event causing the callback
     * to be called.
     **************************************************************/
    XbaeMatrixSelectCellCallbackStruct          *cbs = (XbaeMatrixSelectCellCallbackStruct *) cad;

    fprintf(stderr,"Row %d, Col %d Value: %s\n",cbs->row, cbs->column, XbaeMatrixGetCell(w,cbs->row,cbs->column));
    return;
}
