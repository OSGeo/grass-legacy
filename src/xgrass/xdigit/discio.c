/*******************************************************************************
* discio.c  This file contains all routines which do/control disc input/output *
*      operations for LTPlus.   These include:                                 *
*         openf()           opens files                                        *
*         closef()          closes files                                       *
*         reada()           reads a file ascii-wise (lf, eof, or #bytes read)  *
*         reada_old()       reads a file ascii-wise (but slower)               *
*                                                                              *
*     The primary reason from grouping all of these functions into a single    *
*   block of code is to share global variables in order to track which files   *
*   are left open accidently & to speed reading of ascii (data sensitive)      *
*   files.     file grouped 1/2/91, JPD                                        *
*******************************************************************************/

#include "ginput.h"
#include <string.h>

int  file_pointer[60];             /** pointers to all currently open files **/

char readblock[5][1028];           /** 1k blocks of data used for reada()   **/
int  fileptr[5] = {-1,-1,-1,-1,-1};/** file pointers for each readblock    **/
char *readptr[5][2];               /** next & end pointers for each readblock**/


int openf();
int closef();
char *reada();
char *reada_old();


/*******************************************************************************
* openf                (see manual, Internal Functions)                 <in>   *
*                       Opens a file based on the type parameter.  See the     *
*                       ../../incl/define.h file for type choides.             *
*                                                                              *
*                       Returns  -1  upon abnormal end-of-job (EOJ), else      *
*                       returns  the opened file descriptor.                   *
*                  HLM&JPD030789                                               *
*******************************************************************************/

int openf( fname, type )    
    char *fname;
    int type;
{
/*    char string[256];     */
    int fd;
    long i;

    fd = open(fname, type);
    for(i=0; file_pointer[i]>0; i++);
    file_pointer[i] = fd;
    return( fd );
}


/*******************************************************************************
* closef               (see manual, Internal Functions)                 <in>   *
*                       Close an open file descriptor.                         *
*                                                                              *
*                       Returns zero upon normal end-of-job (EOJ), else        *
*                       returns -1 and sets errno.                             *
*                  HLM&JPD030789                                               *
*******************************************************************************/


int closef( fd )
    int fd;
{
/*    char string[256];   */
    long i;

    if(fd >= 0)
    {
        /** check for & reset any file pointers to reada data  **/
        for(i=0; i<5; i++)
        {    if(fileptr[i] == fd)
                fileptr[i] = -1;
        }

        for(i=0;(file_pointer[i] != fd) && (i<60); i++);
        if(i<60)
            file_pointer[i] = -1;
        else
            fprintf (stderr, 
            "  Attempting to close a file NOT opened using openf().");
        return( close(fd) );
    }
    else
        return(-1);
}


/*******************************************************************************
*    reada        (see manual, Internal Functions)                <in>   *
*            Reads an ascii file using low level I/O.  When a       *
*            newline is read or if "bytes" number of bytes have     *
*            been read, a null is appended and the string returned. *
*                                                                   *
*            Reads file in blocks of 1024 characters for faster response   *
*            Heavily modified 1/2-3/91, JPD                         *
*            HLM&JPD030789                           *
*******************************************************************************/

char *reada( string, bytes, fd  )
    char *string;    
    int bytes;
    int fd;
{
    long brd;
    long p;

    char *ptr, *endptr;


        /*** init string ***/
    *string = (char) 0;

        /**** check for legal file pointer **/
    if(fd < 0)
        return((char *) 0);

        /**** see if file pointer is in list fileptr[], & add if necessary ****/
    for(p=0; p<5; p++)
    {    if(fileptr[p] == fd)
            break;
    }
    if(p > 4)                        /*** not in list ***/
    {        /***  find an unused ptr ***/
        for(p=0; p<5; p++)
        {    if(fileptr[p] < 0)
                break;
        }

        if(p < 5)         /** found an unused pointer **/
        {    fileptr[p] = fd;
            readptr[p][0] = readptr[p][1];
        }
        else              /** all pointers in use    **/
            return(reada_old(string,bytes,fd));     /** read it the old way. **/
    }

        /**** transfer data from readblock to parameter string *****/
    ptr = string;
    endptr = ptr + bytes;
    for(;;)
    {        /**** make sure that there is enough data ***/
        if(readptr[p][0] >= readptr[p][1])
        {        /*** read another block of data ***/
            brd = read(fd, readblock[p],1024);
            if(brd <= 0)
            {    if(ptr == string)
                    return((char *) 0);
                *ptr = (char) 0;
                return(string); 
            }
                /*** set pointers ***/
            readptr[p][0] = readblock[p];
            readptr[p][1] = readptr[p][0] + brd;
        }

            /**** transfer one character ****/
        *ptr = *readptr[p][0];
        readptr[p][0]++;
/*        readptr[p][0] = readptr[p][0] + 1;   */
        
            /**** check for lf ****/
        if(*ptr == '\n')  /** line feed (lf) **/
        {    ptr++;             /** move past lf & return string **/
            *ptr = (char) 0;
            break;
        }

            /**** check for bytes read ****/
        ptr++;
        if(ptr >= endptr)
        {    *ptr = (char) 0;
            break;
        }
    }

        /**** return ***/
    return( string );
}

/*******************************************************************************
*    reada        (see manual, Internal Functions)                <in>   *
*            Reads an ascii file using low level I/O.  When a       *
*            newline is read or if "bytes" number of bytes have     *
*            been read, a null is appended and the string returned. *
*            Reads file one character at a time (slow due to heavy bus use JPD) *
*                                                                   *
*            Returns string upon normal end-of-job (EOJ), else      *
*            returns (char *)0 upon error.                          *
*            HLM&JPD030789                           *
*******************************************************************************/

char *reada_old( string, bytes, fd  )
    char *string;    
    int bytes;
    int fd;
{
    long i, brd;

    for(bytes--, i=0; (brd = read(fd, (string + i), 1)) && i < bytes; i++ )
    {    if( brd == -1 )
            return(0);
        if( *(string + i) == '\n' )
        {
            *(string + i + 1) = '\0';    /* null term string  */
            break;
        }
        else if( i == bytes)
        {
            *(string + i) = '\0';
            break;
        }
    }

    if( brd == 0 )
        return((char *) 0);
    else
        return( string );
}
