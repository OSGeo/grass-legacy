/*
 * Modified for the new Grass 5.0 floating point and
 * null values raster file format.
 * Pierre de Mouveaux - 20 april 2000.
 */

/* lines.c */
int join_lines(struct COOR *, struct COOR *);
int extend_line(struct COOR *, struct COOR *);
int stop_line(struct COOR *, struct COOR *);
struct COOR *get_ptr(void);

