/*======================================================================
Filename:   fileseq.c
Module:	    i.landsat.tm
Author:	    Christopher Lesher

Rules for filenames:
   If file is a device, then use it and prompt.
   If file.ls0 exists, then use it and do not prompt.
   Otherwise, always prompt.  In this case, there is no need for
      the input= on the command line.  The first file is prompted for.
======================================================================*/

#include <sys/types.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include "gis.h"
#include "fileseq.h"
#ifndef __CYGWIN__
extern int errno;
#else
#include <errno.h>
#endif
#ifdef _NO_PROTO
static int MountTape();
static void EnterFilename();
#else
static int MountTape(boolean);
static void EnterFilename(FileSequence*, int);
#endif


/*======================================================================
			     FileSeqInit

Initialize a file sequence object.

Return Value:
   0 if successful, 1 on error.
======================================================================*/
int FileSeqInit (FileSequence *fileseq, boolean prompt, char *name, char *suffix, int blocksize)
{
   struct stat statbuf;

   if(blocksize == 0)
      blocksize = 512;

   fileseq->fileprompt = prompt;
   fileseq->suffix = G_store(suffix);
   fileseq->name = malloc(FILENAME_LEN);
   if(name) {
      fileseq->base = G_store(name);
      strcpy(fileseq->name, name);
   }
   if( name && stat(fileseq->name, &statbuf) == 0 &&
       (S_ISCHR(statbuf.st_mode) || S_ISBLK(statbuf.st_mode))) {
      fileseq->type = SPECIAL_FILE;
      fileseq->firsttape = True;
      fileseq->bufsize = blocksize > MAXRECORD ? MAXRECORD : blocksize;
      fileseq->buffer = (unsigned char *)malloc(fileseq->bufsize);
      if(fileseq->buffer == 0)
      	 return(1);
      fileseq->buflen = 0;
      fileseq->bufread = 0;
   } else {
      fileseq->type = REG_FILE;
      fileseq->filenum = -1;
   }
   fileseq->fp = NULL;
   fileseq->fd = 0;
   return(0);
}



/*======================================================================
			     FileSeqNext

Position at beginning of next file.  May involve prompting for a new file.

If band >= 0, use it as filenum.

Return Value:
   0 if next file found successfully.
   1 on fatal error.
======================================================================*/
int 
FileSeqNext (FileSequence *fileseq, int band)
{
   char buffer[8192];
   char error[500];

   if(fileseq->type == REG_FILE) {
      /* Close previous file if any */
      if(fileseq->fp)
      	 fclose(fileseq->fp);

      if(! fileseq->fileprompt) {
      	 /* If we are not prompting, then assemble the next filename.  If open
      	    fails, then we will prompt for filenames from this point on.
      	    For quad and full scenes, file numbers need not be sequential. */
      	 while(fileseq->filenum <= 30) {
	    if( band >= 0)
	       fileseq->filenum = band;
	    else
	       ++fileseq->filenum;
	    sprintf(fileseq->name, "%s.%s%d", fileseq->base, fileseq->suffix,
	       fileseq->filenum);
	    fileseq->fp = fopen(fileseq->name, "r");
	    if(fileseq->fp) {
	       fileseq->eofreached = False;
	       return(0);
	    }
	    if( band > 0) {
	       fprintf(stderr, "Could not find band %d (%s)\n", band,
		  fileseq->name);
	       break;
	    }
      	 }
      }
      fileseq->fileprompt = True;

      /* Prompt for next file until we get one that we can open.  User may
      	 enter return to quit. */
      fileseq->fp = NULL;
      while(fileseq->fp == NULL) {
      	 EnterFilename(fileseq, band);
      	 if(fileseq->name[0] == 0)
      	    return(1);
      	 fileseq->fp = fopen(fileseq->name, "r");
      }
   } else if(fileseq->type == SPECIAL_FILE) {
      /* First tape is special case. */
      if(fileseq->firsttape) {
      	 fileseq->fd = 0;
      	 while(fileseq->fd == 0) {
      	    if(MountTape(True))
      	       return(1);
	    fileseq->fd = open(fileseq->name, O_RDONLY);
	 }
      	 fileseq->firsttape = False;
      } else {
      	 /* Read until file is exhausted.  FileSeqRead() will
      	    skip EOF later.  */
      	 while(FileSeqRead(fileseq, buffer, sizeof(buffer)));
      }
      fileseq->buflen = 0;
      fileseq->bufread = 0;
   } else return(1);

   fileseq->eofreached = False;
   fileseq->beginning = True;
   return(0);
}



/*======================================================================
			      FileSeqFF

Fast forward.  For tapes, read the given number of bytes.  For files,
just use fseek().  This function works, sort of.

Return value:
   1 if fast forward done successfully.
   0 if end of file reached.
   -1 if error.
======================================================================*/
int 
FileSeqFF (FileSequence *fileseq, int bytes)
{
   int result;
   if( fileseq->type == REG_FILE) {
      fseek(fileseq->fp, bytes, 1);
      return(feof(fileseq->fp));
   }
   else {
      while( bytes > 0) {
      	 result = FileSeqRead(fileseq, fileseq->buffer,
      	    bytes < fileseq->bufsize ? bytes : fileseq->bufsize);
      	 if(result <= 0) return(result);
      	 bytes -= fileseq->bufsize;
      }
      return(1);
   }
}



/*======================================================================
			     FileSeqRead

Read 'length' number of bytes from tape and store in buffer.

Return value:
   Actual number of bytes read.  May be less than length requested.
   Return zero if end of file is reached.
   Return -1 if a serious error occurs or user asks to exit.
======================================================================*/
int 
FileSeqRead (FileSequence *fileseq, unsigned char *dest, int length)
{
   int numread;
   int dataread = 0;
   int data_left;

   /* For regular files, fread() does all the work. */
   if( fileseq->type == REG_FILE) {
      if(feof(fileseq->fp))
      	 return(0);
      return(fread(dest, 1, length, fileseq->fp));
   }

   /* Once eof reached, never read again until FileSeqNext() called, or else
      we would read into next file. */
   if(fileseq->eofreached)
      return(0);

   while(length) {
      data_left = fileseq->buflen - fileseq->bufread;
      if(data_left > 0) {
      	 /* There is unread data in buffer */
      	 if( data_left >= length) {
      	    /* Request can be satisfied from buffer without reading tape. */
      	    memcpy(dest, fileseq->buffer + fileseq->bufread, length);
      	    dataread += length;
      	    fileseq->bufread += length;
      	    return(dataread);
      	 } else {
      	    /* Copy whats left in buffer */
      	    memcpy(dest, fileseq->buffer + fileseq->bufread, data_left);
      	    dataread += data_left;
      	    length -= data_left;
      	    dest += data_left;
      	    fileseq->bufread = fileseq->buflen;
      	 }
      }
      fileseq->bufread = 0;
      errno=0;
      fileseq->buflen = read(fileseq->fd, fileseq->buffer, fileseq->bufsize);

      /* Error at beginning of file means end of media reached.  Otherwise,
      	 a serious error. */
      if(errno) {
      	 if(fileseq->beginning) {
      	    close(fileseq->fd);
      	    fileseq->fd = 0;
      	    while(fileseq->fd == 0) {
      	       if(MountTape(False))
      	       	  return(-1);
      	       fileseq->fd = open(fileseq->name, O_RDONLY);
      	    }
      	 } else {
      	    fprintf(stderr, "Error reading tape (errno = %d)\n", errno);
      	    return(-1);
      	 }
      }
      if( fileseq->buflen == 0) {
      	 fileseq->eofreached = True;
      	 return(dataread);
      }
   }

   return 0;
}



void FileSeqEnd (FileSequence *fileseq)
{
   if(fileseq->fp)
      fclose(fileseq->fp);
   if(fileseq->fd)
      close(fileseq->fd);
   free(fileseq->buffer);
   free(fileseq->name);
   free(fileseq->suffix);
   free(fileseq->base);
}


/*======================================================================
			      MountTape

Ask the user to insert the next (or first) tape.  Return 1 if user asks
to quit, return 0 if user inserts tape.
======================================================================*/
static int MountTape (boolean first)
{
   char buffer[10];

   fprintf(stderr, "Mount the %s tape and hit return (Enter 'quit' to exit): ",
      first ? "first" : "next");
   fgets(buffer, sizeof(buffer), stdin);
   if(strcmp(buffer, "quit\n") == 0)
      return(1);
   else
      return(0);
}

/*======================================================================
			    EnterFilename

Prompt user for filename and return in fileseq->name.
======================================================================*/
static void EnterFilename (FileSequence *fileseq, int band)
{
   if(band >= 1)
      fprintf(stderr, "Enter filename for band %d (Hit return to quit): ", band);
   else
      fprintf(stderr, "Enter next filename (Hit Return to quit): ");
   fgets(fileseq->name, FILENAME_LEN, stdin);
   fileseq->name[strlen(fileseq->name)-1] = 0;
}
