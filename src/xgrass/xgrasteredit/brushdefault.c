
/* 
 * FILE: brushdefault.c
 *
 * PROGRAMMER: David M. Johnson
 * 
 * FUNCTION:
 *
 * BuildDefaultBrush()
 * -------------------
 * Fills the global brush array (Global.brush) with default 
 * brush cells.  
 *
 */

#include "xgre.h"

void BuildDefaultBrush()
{
int bc, br;
char msg[100];

for (br = 0; br < Global.brushRows; br++)
   {
   for (bc = 0; bc < Global.brushCols; bc++)
      {
      Global.brush[bc][br].input = XGRE_INPCAT;
      Global.brush[bc][br].op    = XGRE_OPCOPY;
      Global.brush[bc][br].value = 0;
      }
   }
sprintf(msg,"BRUSH: %dx%d (default)",Global.brushRows,Global.brushCols);
UpdateBrushText(msg);
}

