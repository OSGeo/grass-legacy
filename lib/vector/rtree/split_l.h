/****************************************************************************
* MODULE:       R-Tree library 
*              
* AUTHOR(S):    Antonin Guttman - original code
*               Daniel Green (green@superliminal.com) - major clean-up
*                               and implementation of bounding spheres
*               
* PURPOSE:      Multidimensional index
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*****************************************************************************/

/*-----------------------------------------------------------------------------
| Definitions and global variables used in linear split code.
-----------------------------------------------------------------------------*/

#define METHODS 1

struct Branch BranchBuf[MAXCARD+1];
int BranchCount;
struct Rect CoverSplit;

/* variables for finding a partition */
struct PartitionVars
{
	int partition[MAXCARD+1];
	int total, minfill;
	int taken[MAXCARD+1];
	int count[2];
	struct Rect cover[2];
	RectReal area[2];
} Partitions[METHODS];
