struct RCLIST
{
   short row;
   short col;
   CELL val;
} ;
/* utm_to_rc.c */
struct RCLIST *utm_to_rc(struct Cell_head *, double, double, struct RCLIST *, int *);
