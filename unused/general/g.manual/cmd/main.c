#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "gis.h"

/* the maximum section number used */
#define MAX_SECTIONS 5

char *find_man_page(char *, int *);
int display_man_page(char *);
char *section_name(int);
int list_all(int, int, int);
int list_all_tty(int, int, int);
int list_all_not_tty(int, int, int);

int main (int argc, char **argv)
{
    int i, exit_code;
    struct Option *name;  /* list of man pages to view               */
    struct Flag *aflag;   /* list all manual entries (pretty format) */
    struct Flag *oneflag; /* list all manual entries                 */
    struct Flag *fflag;   /* add formfeeds to pretty output          */
    struct Flag *eflag;   /* ignore empty man sections               */
    struct Flag *sflag;   /* run silently checking existence         */

    G_gisinit(argv[0]);

    name = G_define_option();
    name->key = "entries";
    name->description = "manual entries to be displayed";
    name->required = NO;
    name->type = TYPE_STRING;
    name->multiple = YES;

    oneflag = G_define_flag();
    oneflag->key = '1';
    oneflag->description = "list all manual entries (one per line)";

    aflag = G_define_flag();
    aflag->key = 'a';
    aflag->description = "list all manual entries (pretty format)";

    eflag = G_define_flag();
    eflag->key = 'e';
    eflag->description = "ignore emtpy manual sections";

    fflag = G_define_flag();
    fflag->key = 'f';
    fflag->description = "add formfeeds to output (pretty format mode)";

    sflag = G_define_flag();
    sflag->key = 's';
    sflag->description = "run silently (set exit status if entry exists)";

    if (G_parser(argc,argv)) {
        exit(1);
    }

    /* only one [-a,-1,-s] flag at a time !! */
    if ( (aflag->answer + sflag->answer + oneflag->answer) > 1 ) {
        fprintf(stderr,"Error: only one of [-a,-1,-s] can be specified\n");
        G_usage();
        exit(1);
    }

    if ( fflag->answer && !aflag->answer ) {
        fprintf(stderr,"Error: must specify -a when using -f\n");
        G_usage();
        exit(1);
    }

    if ( eflag->answer && !(aflag->answer || oneflag->answer) ) {
        fprintf(stderr,"Error: must specify -a or -1 when using -e\n");
        G_usage();
        exit(1);
    }

    /* if the user wants a listing, give it to them and exit */
    if ( aflag->answer || oneflag->answer ) {
        list_all(aflag->answer,fflag->answer,eflag->answer);
        exit(0);
    }

    /* see if all man pages are there, if running silently */
    exit_code = 0;
    for(i = 0; name->answers[i]; i++) {
        int section;
        char *path;

        path = find_man_page(name->answers[i],&section);

        if ( path == NULL ) {
            if ( ! sflag->answer )
                fprintf(stderr,"%s: no such manual entry\n",
                    name->answers[i]);
            exit_code = 1;
        }
    }
    /* if non-zero exit code, exit */
    if ( exit_code ) exit(exit_code);
    /* if running silently, exit with zero exit code */
    if ( sflag->answer ) exit(exit_code);

    /* now go through them again displaying them */
    for(i = 0; name->answers[i]; i++) {
        int section; /* not used currently */

        display_man_page(find_man_page(name->answers[i],&section));
    }
    exit(0);
}

char *find_man_page (char *entry, int *sec)
{
    static char buf[1024];
    int section = 0;

    for ( section = 1; section <= MAX_SECTIONS; section++) {
        sprintf (buf, "%s/man/%d/%s", G_gisbase(), section, entry);
        if ( access(buf,0) == 0 ) {
            *sec = section;
            return buf;
        }
    }
    return NULL;
}

int display_man_page (char *path)
{
    char buf[1024];

    if (isatty(1))
         sprintf(buf,"$PAGER %s",path);
    else 
	 sprintf(buf,"cat %s",path);
    G_system(buf);

    return 0;
}


	/*
	 *	get_section_name(int)
	 *
	 *	Return the formal name of the manual section specified
	 *	in the manN section (where N is the value passed to this
	 *	routine).  This name is obtained by reading it in from
	 *	the file "N/.class-title".
	 *
	 *	Modification Request:	GRASS920119, 1992-12-16
	 *				Amit Parghi
	 */
#define MAX_INPUT_LENGTH 80

char *section_name (int section)
{
    static char value[MAX_INPUT_LENGTH] = "";	/* return value */
    char filename[128];
    FILE *fp;

    sprintf(filename, "%s/man/%d/.class-title", G_gisbase(), section);

    if ((fp = fopen(filename, "r")) == NULL) {		/* couldn't open */
	sprintf(value, "Section %d\n", section);	/* default */
		/* should we bother calling perror() here?? */
	perror(filename);

    } else {
	do {
	    fgets(value, MAX_INPUT_LENGTH, fp);
	} while (value[0] == '\0' || value[0] == '#');
	fclose(fp);
    }

    return value;
}

int list_all (int pretty, int fflag, int eflag)
{
    char buf[1024];
    char *tempfile, *last, *temp1, *temp2, *temp ;
    FILE *line, *head, *tmp;
    struct stat statbuf;

    if(isatty(1)) 
       {
	  fprintf(stderr, "One moment...\n");
	  list_all_tty(pretty, fflag, eflag);
       }
    else
	  list_all_not_tty(pretty, fflag, eflag);

    return 0;
}


int list_all_tty (int pretty, int fflag, int eflag)
{
    char buf[1024];
    char *tempfile, *last, *temp1, *temp2, *temp ;
    FILE *line, *head, *tmp;
    struct stat statbuf;
    int section;

    tempfile = G_tempfile();
    temp = G_tempfile();
    tmp = fopen(tempfile, "w");
    fprintf(tmp, "");
    fclose(tmp);
    for ( section = 1; section <= MAX_SECTIONS; section++ ) {
        last = G_tempfile();
        if ( pretty ) 
	{
            temp1 = G_tempfile();
            temp2 = G_tempfile();
	    line = fopen(temp1, "w");
	    head = fopen(temp2, "w");
            fprintf(head, section_name(section));
	    fclose(head);
            fprintf(line,"------------------------------------------------------------------------------\n");
	    fflush(line);
	    fclose(line);
            sprintf(buf,"ls -C %s/man/%d > %s", G_gisbase(), section, temp);
            G_system(buf);
	    sprintf(buf,"cat %s %s %s %s > %s", temp2, temp1, temp, temp1,
		     last);
            if(G_system(buf)){
               fprintf (stdout,"%s\n", buf );
               G_system(buf);}
        } 
	else 
	{
            sprintf(buf,"ls %s/man/%d > %s", G_gisbase(), section, last);
            G_system(buf);
        }

        /* eflag ? check to see if last is empty */
        if ( eflag ) 
	{
            if ( stat(last,&statbuf) != 0 ) 
	    {
                fprintf(stderr,"Can't stat temporary file\n");
                exit(1);
            } 
	    else 
	    {
                if ( statbuf.st_size > 0 ) 
		{
		    sprintf(buf,"cat %s %s > %s ", tempfile,last,temp);
                    G_system(buf);
		    sprintf(buf, "cp %s %s", temp, tempfile);
                    G_system(buf);
                }
            }
        } 
	else 
	{
	    if ( pretty && fflag ) 
	    {
                temp2 = G_tempfile();
	        line = fopen(temp2, "w");
		fflush(line);
		if ( section != MAX_SECTIONS )
                     fprintf(line,"\f\n");
                fclose(line);
		sprintf(buf, "cat %s %s > %s", last, temp2, temp);
                G_system(buf);
		sprintf(buf, "cp %s %s", temp, last);
                G_system(buf);
	    }
	    sprintf(buf,"cat %s %s > %s",tempfile, last, temp);
            G_system(buf);
            sprintf(buf, "cp %s %s", temp, tempfile);
            G_system(buf);
        }
    }
	unlink(last);
	if(pretty) unlink(temp1);
	if(pretty) unlink(temp2);
	free(last);
	if(pretty)free(temp1);
	if(pretty)free(temp2);
    if(isatty(1))
         sprintf(buf,"$PAGER %s",tempfile);
    else
	 sprintf(buf,"cat %s",tempfile);
    G_system(buf);
    unlink(tempfile);
    free(tempfile);
    unlink(temp);
    free(temp);

    return 0;
}

int 
list_all_not_tty (int pretty, int fflag, int eflag)
{
    char buf[1024];
    char *tempfile;
    struct stat statbuf;
    int section;

    for ( section = 1; section <= MAX_SECTIONS; section++ ) {
        tempfile = G_tempfile();
        if ( pretty ) {
            sprintf(buf,"ls -C %s/man/%d > %s", G_gisbase(), section, tempfile);
        } else {
            sprintf(buf,"ls %s/man/%d > %s", G_gisbase(), section, tempfile);
        }
        G_system(buf);
        /* eflag ? check to see if tempfile is empty */
        if ( eflag ) {
            if ( stat(tempfile,&statbuf) != 0 ) {
                fprintf(stderr,"Can't stat temporary file\n");
                exit(1);
            } else {
                if ( statbuf.st_size > 0 ) {
		    if ( pretty ) {
			fprintf(stdout, section_name(section));
			fprintf(stdout,"-------------------------------------");
			fprintf(stdout,"-----------------------------------\n");
			fflush(stdout);
		    }
		    if(isatty(1))
                       sprintf(buf,"$PAGER %s",tempfile);
                    else
		       sprintf(buf,"cat %s",tempfile);
                    G_system(buf);
		    if ( pretty && fflag ) {
			fflush(stdout);
			if ( section != MAX_SECTIONS )
			    fprintf(stdout,"\f\n");
		    }
                }
            }
        } else {
	    if ( pretty ) {
		fprintf(stdout, section_name(section));
		fprintf(stdout,"-------------------------------------");
		fprintf(stdout,"-----------------------------------\n");
		fflush(stdout);
	    }
	    if(isatty(1))
                 sprintf(buf,"$PAGER %s",tempfile);
            else
		 sprintf(buf,"cat %s",tempfile);
            G_system(buf);
	    if ( pretty && fflag ) {
		fflush(stdout);
		if ( section != MAX_SECTIONS )
		    fprintf(stdout,"\f\n");
	    }
        }
	unlink(tempfile);
	free(tempfile);
    }

    return 0;
}
