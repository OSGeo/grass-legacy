/*======================================================================
Filename:   fileseq.h
Module:	    i.landsat.tm
Author:	    Christopher Lesher
======================================================================*/
#ifndef _fileseq_h
#define _fileseq_h

#include <stdio.h>

#define True   	     	1
#define False  	     	0
#define REG_FILE     	0
#define SPECIAL_FILE 	1
#define FILENAME_LEN 	100
#define MAXRECORD (1024*1024) /* Maximum size of a single read() from tape. */
#define DEFAULT_RECORD	32768 /* Default value of blocksize */

typedef unsigned char boolean;

typedef struct {
   boolean  fileprompt;	/* True if we should prompt for the next file. */
   FILE	    *fp;       	/* Currently open regular file */
   int	    fd;	     	/* Currently open device */
   int	    filenum;    /* Number of current file */
   char	    *suffix;    /* Suffix of files, not including . or number */
   char	    *base;   	/* Base name of files. */
   char	    *name;   	/* Full name of currently open file. */
   boolean  eofreached;	/* True once EOF is reached. */
   boolean  beginning;	/* True if we are positioned at start of file */
   boolean  firsttape;	/* True if the first tape needs to be mounted. */
   int	    type;    	/* REG_FILE or SPECIAL_FILE */
   unsigned char *buffer;  /* Holds data read from tape. */
   int	    bufsize; 	/* size of buffer */
   int	    buflen;  	/* total amount of data in buffer, read + unread */
   int	    bufread; 	/* amount of data in buffer already read. */
} FileSequence;

#ifdef _NO_PROTO
int FileSeqInit();
int FileSeqNext();
int FileSeqRead();
void FileSeqEnd();
int FileSeqFF();
#else
int FileSeqInit(FileSequence*, boolean, char *, char *, int);
int FileSeqNext(FileSequence*, int );
int FileSeqRead(FileSequence*, unsigned char *, int);
void FileSeqEnd(FileSequence*);
int FileSeqFF(FileSequence*, int);
#endif

#endif

