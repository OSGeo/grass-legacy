/***********************************************************************

File     	:	cell_edit.h

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:
Abstract 	:       header file for the celeditor
Returns  	:       None.

***********************************************************************/
#include <stdio.h>
#include <math.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include "/home/grass3/src/libes/gis.h"

extern Widget editor;
extern Window busy;
extern char   *cellPath;
extern Boolean loaded;
extern Boolean saved;

/* call back pairs */

/* Global Actions */
extern void DoneProc();
extern void PopDown();
extern void ExitEditor();
extern void QuitProc();
extern void QueryEditor();

/* macros */
#define MIN(x,y) { (x < y) ? x : y; }
#define MAX(x,y) { (x > y) ? x : y; }
#define streq(a,b) (strcmp( (a), (b) ) == 0)
