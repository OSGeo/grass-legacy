struct line_hdr
{
  struct COOR *left;
  struct COOR *right;
  struct COOR *center;
};

struct COOR
{
  struct COOR *bptr, *fptr;		/* pointers to neighboring points */
  int row, col, node;			/* row, column of point; node flag */
  CELL right, left;			/* areas to right and left of line */
};
