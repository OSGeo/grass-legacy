/*
 * Modified for the new Grass 5.0 floating point and
 * null values raster file format.
 * Pierre de Mouveaux - 20 april 2000.
 */


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
/*    CELL right, left;		 */	/* areas to right and left of line */

};
/* extr_lines.c */
int join_lines(struct COOR *, struct COOR *);
int extend_line(struct COOR *, struct COOR *);
int stop_line(struct COOR *, struct COOR *);
struct COOR *get_ptr(void);
int extract_lines(void);
int alloc_bufs(int);
/* io.c */
int write_line(struct COOR *);
int syntax(int, char *[], char *, char *);
int read_row(void *);
int open_file(char *, char *);
int close_file(void);
int fill_head(void);
void *xmalloc(int, char *);
int xfree(void *, char *);
void *xrealloc(char *, int, char *);
