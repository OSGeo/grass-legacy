
#define DEBUG1

/*
 * FILE: editundo.c
 *
 * PROGRAMMER: David M. Johnson 
 *
 * FUNCTIONS:
 *
 * The functions in this file concern the undo-buffer, which is
 * actually a stack implemented by use of the generic linked
 * list functions in llist.c. 
 *
 * EditUndo()
 * ----------
 * This function "undoes" all of the edits that have occured
 * since the last time it was called or the last time that
 * the function ClearUndoBuffer() was called.  It does this by
 * popping the original cell values off of the undoList
 * stack and putting them back into the raster map. 
 *
 * AddToUndoBuffer(row,col,val)
 * ----------------------------
 * This function pushes an "original" raster map cell onto the 
 * undoList stack.
 *
 * ClearUndoBuffer()
 * -----------------
 * This function frees all of the memory associated with the
 * undoList stack. 
 *
 */

#include "xgre.h"
#include "ll.h"

llist *undoList;    /* stack of cell modifications */

typedef struct _cell_mod
   {
   int row;         /* row coord of modified cell */
   int col;         /* column coord of modified cell */
   CELL val;        /* original value of cell */
   } CellMod;

/****************/
/*** EditUndo ***/
/****************/

int EditUndo()
{
CellMod *tmp;

if (undoList == NULL) return(-1);

/* loop replace original cell values */
XgDoHourGlass(Global.applShell);
while ((tmp = (CellMod *)LLpopEnd(undoList)) != NULL)  
   {
#  ifdef DEBUG
   printf("popped (%d,%d) %ld\n",tmp->col,tmp->row,tmp->val);
#  endif
   if (!segment_put(&(Global.seg),(char*)&(tmp->val),tmp->row,tmp->col))
      XgError(Global.applShell,"Error writing to segment file");
   free(tmp);
   }

/* FIX: adjust range min and max */
/* FIX: reset category structure min and max */
/* FIX: reset color structure min and max */

RedrawImage();
if (Global.zoomLoaded) zoomDraw();
XgUndoHourGlass(Global.applShell);
}

/***********************/
/*** AddToUndoBuffer ***/
/***********************/

int 
#ifdef _NO_PROTO
AddToUndoBuffer(row,col,val)
   int row;
   int col;
   CELL val;
#else
AddToUndoBuffer(int row, int col, CELL val)
#endif
{
CellMod *mod;
int count;

if (undoList == NULL) undoList = LLinit();

mod = (CellMod *)malloc(sizeof(CellMod));
mod->row = row;
mod->col = col;
mod->val = val;
count = LLpushEnd(undoList,(unsigned char *)mod);

#ifdef DEBUG
printf("pushed mod#%d\n",count);
#endif

return(count);
}

/***********************/
/*** ClearUndoBuffer ***/
/***********************/

void ClearUndoBuffer()
{
LLfreeList(undoList);
undoList = NULL;
}


