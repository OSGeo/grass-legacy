#include <grass/segment.h>

#define DSEG struct _d_s_e_g_
DSEG {
    SEGMENT seg;		/* segment structure */
    int fd;			/* fd for reading/writing segment file */
    char *filename;		/* name of segment file */
    char *name;			/* raster map read into segment file */
    char *mapset;
};

#define BSEG struct _b_s_e_g_
BSEG {
    SEGMENT seg;		/* segment structure */
    int fd;			/* fd for reading/writing segment file */
    char *filename;		/* name of segment file */
    char *name;			/* raster map read into segment file */
    char *mapset;
};

/* bseg_close.c */
int bseg_close(BSEG *);

/* bseg_get.c */
int bseg_get(BSEG *, char *, int, int);

/* bseg_open.c */
int bseg_open(BSEG *, int, int, int);

/* bseg_put.c */
int bseg_put(BSEG *, char *, int, int);

/* bseg_read.c */
int bseg_read_cell(BSEG *, char *, char *);

/* bseg_write.c */
int bseg_write_cellfile(BSEG *, char *);

/* dseg_close.c */
int dseg_close(DSEG *);

/* dseg_get.c */
int dseg_get(DSEG *, int, int, DCELL *);

/* dseg_open.c */
int dseg_open(DSEG *, int, int, int);

/* dseg_put.c */
int dseg_put(DSEG *, int, int, DCELL);

/* dseg_read.c */
int dseg_read_cell(DSEG *, char *, char *);

/* dseg_write.c */
int dseg_write_cellfile(DSEG *, char *);
