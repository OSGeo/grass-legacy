#ifndef SEEK_OFFSET
#define SEEK_OFFSET long
#endif

typedef struct
{
    int open;		/* open flag */
    int nrows;		/* rows in original data */
    int ncols;		/* cols in original data */
    int len;            /* bytes per data value */
    int srows;          /* rows in segments */
    int scols;          /* cols in segments */
    int size;		/* size in bytes of a segment */
    int spr;		/* segments per row */
    int spill;		/* cols in last segment in row */
    int fd ;            /* file descriptor to read/write segment */
    struct SEGMENT_SCB	/* control blocks */
    {
	char *buf ;	/* data buffer */
	char dirty ;	/* dirty flag */
	int age;	/* for order of access */
	int n;		/* segment number */
    } *scb ;
    int nseg;		/* number of segments in memory */
    int cur;		/* last accessed segment */
    SEEK_OFFSET offset;	/* offset of data past header */
} SEGMENT ;

SEEK_OFFSET lseek();
