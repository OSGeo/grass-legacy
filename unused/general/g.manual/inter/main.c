#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include "gis.h"

int do_printer(char *);
int oops(char *);

int main (int argc, char **argv)
{
    char entry[1024];
    char buf[1024];
    char *tempfile;

    G_gisinit (argv[0]);


    G_clear_screen();
    fprintf (stderr,"GRASS online manual\n");

    while (1)
    {
        do
        {
            fprintf(stderr,"\n");
            fprintf(stderr,"Which manual entry would you like to see?\n");
            fprintf(stderr,"Enter \"list\" for a list of manual entries\n");
            fprintf(stderr,"Hit RETURN to quit\n");
            fprintf(stderr,"> ");
        }   while (!G_gets(entry));
        if (!strncmp (entry, "list", 4)) {
            tempfile = G_tempfile();
            sprintf(buf,"g.manual -af > %s",tempfile);
            G_system(buf);
            sprintf(buf,"$PAGER %s",tempfile);
            G_system(buf);
            do_printer(tempfile);
        } else if ( !strcmp(entry,"")) {
	    exit(0);
	} else {
            sprintf(buf,"g.manual -s '%s'",entry);
	    if (G_index(entry,'\'') != NULL | G_index(entry,'\\') != NULL ) {
		oops(entry);
	    } else if ( G_system(buf) == 0 ) {
                tempfile = G_tempfile();
                sprintf(buf,"g.manual %s > %s",entry,tempfile);
                G_system(buf);
                sprintf(buf,"$PAGER %s",tempfile);
                G_system(buf);
                do_printer(tempfile);
            } else {
		oops(entry);
            }
        }
    }

    return 0;
}

int do_printer (char *s)
{
    char buf[1024];
    char print[1024];

    do {
        fprintf(stderr,
	    "Would you like to send this list to the printer ? [n] ");
    } while ( !G_gets(print));
    if ( !strcmp(print,"")) {
	unlink(s);
	free(s);
	return 0;
    }
    if ( !strncmp(print,"y",1) || !strncmp(print,"Y",1 ) ) {
	do {
	    fprintf(stderr,
	        "Please Enter printer command [lpr]: ");
	} while ( !G_gets(print));
	if ( !strcmp(print,"")) {
	    sprintf(buf,"cat %s | lpr",s);
	    G_system(buf);
	} else {
	    sprintf(buf,"cat %s | %s",s,print);
	    G_system(buf);
	}
    }
    unlink(s);
    free(s);

    return 0;
}

int oops (char *s)
{
    fprintf(stderr,"%s - no such manual entry\n\n",s);

    return 0;
}
