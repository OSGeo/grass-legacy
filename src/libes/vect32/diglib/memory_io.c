/*
**  Written by: Dave Gerdes 5 1988
**  US Army Construction Engineering Research Lab
*/

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "Vect.h"


/* this code is an attempt to speed up execution of parts of the dig_import
**    program.  basically I attempt to load the entire digit file into memory
**    and pay for swapping instead of thousands of I/O calls
** so far I have seen dramatic speedups of execution on the masscomps.
** If this is not desired, just comment out the call to dig_Mem_load_file()
** in import/main.c
**
**  
**  this code can be used by an routines that call dig__Read_line ()
** as long as they do NOT call dig_Write_line() in an attempt to append
** to the end of the file.  (I did not support reallocing that space for
** fairly obvious reasons.)
*/

/* 
** mread (buf, sizeof element, num elems, memory pointer 
**
**   fread equivalent, uses Mem_Line_Ptr as begining of virtual file
**
**   returns  -1 on error
**   	       0 EOF?
**	      bytes read
*/


/* 
**  attempt to read an entire file into memory
**
**   returns  0 on success
**   returns -1 on error
**
*/
int dig_Mem_load_file (
    FILE *fp,
    char **ptr)
{
#ifndef MEMORY_IO	/* not upgraded for 4.0 yet */

    return -1;
#else
    unsigned size;
    unsigned int bufsize;
    unsigned int amt_read;
    struct stat sbuf;
    int ret;

    bufsize = 10 * BUFSIZ;

    if (Lines_In_Memory)
    {
	Lines_In_Memory = 0;
	if (Mem_Line_Ptr != NULL)
	{
	    free (Mem_Line_Ptr);
	}
    }
    fstat (fileno (fp), &sbuf);
    size = sbuf.st_size;
/*DEBUG*/ debugf ("Mallocing %d bytes for file\n", size);
    *ptr = malloc (size);
    if (*ptr == NULL)
	return (-1);
/*DEBUG*/ debugf ("Malloc successful \n", size);
    amt_read = 0;
    fseek (fp, 0L, 0);
    while (size - amt_read > 0)
    {
	if (size - amt_read < bufsize) 
	    bufsize = size - amt_read;
	if (0 > (ret = fread (*ptr + amt_read, bufsize, 1, fp)))
	{
/*DEBUG*/ debugf ("FREAD FAILED!\n");
	    free (*ptr);
	    return (-1);
	}
	if (ret == 0)
	    break;
	amt_read += bufsize;
/*DEBUG*/ debugf ("AMT_READ = %d\n", amt_read);
    }
/*DEBUG*/ debugf ("Read %d bytes into memory\n", amt_read);
    Lines_In_Memory = 1;
    Mem_Line_Ptr = *ptr;
    Mem_curr_position = Mem_Line_Ptr;

/*DEBUG*/ debugf ("Load completed successfully\n");
    return (0);
#endif  /* MEMORY_IO */
}


#ifdef MEMORY_IO		/* not upgraded for 4.0 yet */
int dig_Mem_release_file ()
{
    if (Lines_In_Memory)
    {
	Lines_In_Memory = 0;
	if (Mem_Line_Ptr != NULL)
	    free (Mem_Line_Ptr);
	Mem_Line_Ptr = NULL;
    }

    return 0;
}

int dig_mread (
    char *buf,
    unsigned int size,
    unsigned int n_elem,
    int mp)
{
    if (!Lines_In_Memory)
	return (0);
    if (Mem_Line_Ptr == NULL)
	return (0);
    
    dig_struct_copy (Mem_curr_position, buf, size * n_elem);
    Mem_curr_position += size * n_elem;
    return (1);
}

int dig_mwrite (
    char *buf,
    unsigned int size,
    unsigned int n_elem,
    int mp)
{
    if (!Lines_In_Memory)
	return (0);
    if (Mem_Line_Ptr == NULL)
	return (0);
    
    dig_struct_copy (buf, Mem_curr_position, size * n_elem);
    Mem_curr_position += size * n_elem;
    return (1);
}

int dig_mseek (
    int mp,
    long offset)
    int code)
{
    switch (code) {
	case 0:
	    Mem_curr_position = offset + Mem_Line_Ptr;
	    break;
	case 1:
	    Mem_curr_position += offset;
	    break;
	case 3:
	default:
	    return (-1);
	    break;
    }
    return (0);
}
#endif	/* MEMORY_IO */
