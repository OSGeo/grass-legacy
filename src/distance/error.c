/* %W% %G% */

#include "gis.h"

static char mapname[20] ;
extern int Mail;

set_error_msg(msg)
    char *msg ;
{
    strcpy(mapname, msg) ;
}

dist_error(msg, fatal)
    char *msg ;
    int fatal ;
{
    FILE *popen() ;
    FILE *mailpipe ;
    char buffer[80] ;

    if (Mail)
    {
	sprintf(buffer, "mail '%s'", G_whoami()) ;
	mailpipe = popen(buffer, "w") ;
	if (fatal)
	    fprintf(mailpipe, "Fatal error message from distance:\n") ;
	else
	    fprintf(mailpipe, "Message from distance:\n") ;
	fprintf(mailpipe, "\nAnalysis on mapname [%s]\n", mapname) ;
	fprintf(mailpipe, "%s\n", msg) ;
	pclose(mailpipe) ;
    }
    else
    {
	fprintf(stderr, "\nDistance analysis on mapname [%s]\n", mapname) ;
	if (fatal)
	    G_fatal_error (msg);
	else
	    fprintf (stderr, "%s\n", msg);
    }
}
