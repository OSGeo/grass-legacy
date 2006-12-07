#ifndef GREGION_LOCAL_PROTO_H
#define GREGION_LOCAL_PROTO_H
/* adjust.c */
int adjust_window(struct Cell_head *, int, int, int);
/* zoom.c */
int zoom(struct Cell_head *, char *, char *);
/* printwindow.c */
int print_window(struct Cell_head *,int, int, int, int, int);
#endif
