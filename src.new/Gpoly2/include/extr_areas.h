/* COOR - structure to store points from cell file in linked list */

struct COOR
{
  struct COOR *bptr, *fptr;		/* pointers to neighboring points */
  int row, col, node;			/* row, column of point; node flag */
  CELL right, left;			/* areas to right and left of line */
};

#define NULPTR ((struct COOR *) NULL)

/* area_table - structure to store stuff associated with each */
/* area number */

struct area_table
{
  int free;				/* this entry is not taken yet */
  CELL cat;				/* category number for this area */
  int row;				/* row and column of point where the */
  int col;				/*   area is widest */
  int width;				/*   and width there */
};

/* equiv_table - structure in which to compile equivalences between area */
/* numbers */

struct equiv_table
{
  int mapped;				/* is this area number mapped? */
  int where;				/* if so, where */
  int count;				/* if not, number mapped here */
  int length;
  int *ptr;				/*   and pointer to them */
};
