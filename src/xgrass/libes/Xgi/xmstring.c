static char rcsid[] = "@(#)XGRASS $Id: xmstring.c,v 0.0 1992/05/05 14:56:29 sink Exp sink $";
/*
 * File: xmstring.c
 *
 * Desc: This file contains the function XgCreateXmStringFromFile()
 *       which opens a file in $XGRASSHELPDIR/xgrass and creates an returns
 *       an XmString with the contents. The user is responsible for freeing 
 *       the XmString. Returns NULL if an error.
 *
 * Auth: Kurt Buehler
 *
 * Date: Mon Feb 17 08:33:13 CST 1992 (Presidents Day)
 *
 * Modification History:
 *
 *
 */
#include "xgrass_lib.h"

XmString
#ifdef _NO_PROTO
XgCreateXmStringFromFile(file)
char *file;
#else
XgCreateXmStringFromFile( char *file)
#endif
{
    XmString xms;
    char buf[1024];
    int length;
    int bytes;
    struct stat statbuf;
    FILE *fp;
    char *libdir;
    char *string;
    char *ptr;

    if ( file == NULL || *file == NULL )
	return NULL;

    if ( (libdir = (char *)getenv("XGRASSHELPDIR")) == NULL ) {
	fprintf(stderr,"XGRASSHELPDIR not set\n");
	return NULL;
    }

    sprintf(buf,"%s/xgrass/%s", libdir, file);

    if ( (fp = fopen(buf, "r")) == NULL ) {
	fprintf(stderr,"Can't open %s\n", buf);
	return NULL;
    }

    if ( stat(buf,&statbuf) == 0 )
	length = statbuf.st_size;
    else
	length = 10000000;

    if ( length > 0 ) {
	string = (char *)XtMalloc(length + 1);
	bytes = fread(string,sizeof(char), length, fp);
	string[bytes] = '\0';
    }
    ptr = string;
    while ( *ptr ) {
	if ( *ptr == '\n' )
	    *ptr = '\012';
	else if ( !isprint(*ptr) )
	    *ptr = ' ';
	ptr++;
    }
    fclose(fp);

    xms = XmStringCreateLtoR(string,XmSTRING_DEFAULT_CHARSET);
    XtFree(string);
    return xms;
}
