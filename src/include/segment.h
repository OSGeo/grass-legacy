#ifndef GRASS_SEGMENT_H
#define GRASS_SEGMENT_H

#include "config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifndef SEEK_OFFSET
#define SEEK_OFFSET off_t
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

#ifndef HAVE_LSEEK
SEEK_OFFSET lseek();
#endif

#ifndef _GRASS_GIS_LIB
#include "gis.h"
#endif

int segment_address ( SEGMENT *, int, int, int *, int *);
int segment_flush ( SEGMENT *);
int segment_format (int, int, int, int, int, int);
int segment_format_nofill (int, int, int, int, int, int);
/*  int segment_get( SEGMENT *,int *, int, int); */
int segment_get( SEGMENT *,void *, int, int);
/*  int segment_get_row (SEGMENT *, CELL *, int); */
int segment_get_row (SEGMENT *, void *, int);
int segment_init ( SEGMENT *, int, int);
int segment_pagein (SEGMENT *,int);
int segment_pageout ( SEGMENT *,int);
/*  int segment_put ( SEGMENT *,int *,int,int); */
int segment_put ( SEGMENT *,void *,int,int);
/*  int segment_put_row (SEGMENT *,CELL *,int); */
int segment_put_row (SEGMENT *,void *,int);
int segment_release (SEGMENT *);
int segment_seek ( SEGMENT *, int, int);
int segment_setup (SEGMENT *);
#endif
