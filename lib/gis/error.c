/*
 **********************************************************************
 *
 * G_fatal_error (msg)
 *      char *msg          One line error message
 *
 * G_warning (msg)
 *      char *msg
 *
 *   Gives the message, msg, to the user.  Also print a message to 
 *   $GISBASE/GIS_ERROR_LOG if the file exists and is writeable; 
 *   and to the file $HOME in the users home directory if $HOME is set.
 *   G_warning() returns, which G_fatal_error() exits.
 *
 *   note:  By default, the message is handled by an internal routine
 *      which prints the message to the screen.   Using G_set_error_routine()
 *      the programmer can have the message handled by another routine.
 *      This is especially useful if the message should go to a particular
 *      location on the screen when using curses or to a location on
 *      a graphics device (monitor).
 *          
 **********************************************************************
 * G_set_error_routine (error_routine)
 *      int (*error_routine)()
 *
 *   Establishes error_routine as the routine that will handle 
 *   the printing of subsequent error messages. error_routine
 *   will be called like this:
 *      error_routine(msg, fatal)  
 *         char *msg ;
 **********************************************************************
 * G_unset_error_routine ()
 *
 *   After this call subsequent error messages will be handled in the
 *   default method.  Error messages are printed directly to the
 *   screen:
 *           ERROR: message
 *   -or-    WARNING: message
 *
 **********************************************************************/
/*
     Throughout the code the references to these routines attempt to
	send format strings and arguments.  It seems that we expect
	these to handle varargs, so now they do.    7-Mar-1999
		Bill Hughes
*/

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <stdarg.h>
#include <sys/types.h>
#include "glocale.h"
#include "gis.h"

#define MSG  0
#define WARN 1
#define ERR  2

/* static int (*error)() = 0; */
static int (*ext_error)() = 0; /* Roger Bivand 17 June 2000 */
static int no_warn = 0;
static int no_sleep = 1;
static int message_id = 1;

extern char *getenv();
static int print_word ( FILE *,char **,int *,int);
static void print_sentence ( FILE *, int type, char *);
static int print_error(char *,int);
static int mail_msg (char *,int);
static int write_error(char *, int,char *,long,char *);
static int log_error (char *,int);

/*!
 * \brief Print a message to stderr
 *
 * The output format depends on enviroment variable GRASS_MESSAGE_FORMAT
*/
void G_message (char *msg,...)
{
    char buffer[2000];  /* G_asprintf does not work */
    va_list ap;

    va_start(ap, msg);
    vsprintf(buffer,msg,ap);
    va_end(ap);

    print_error (buffer,MSG);
}

int G_fatal_error ( char *msg,...)
{
    char buffer[2000];  /* No novels to the error logs, OK? */
    va_list ap;

    va_start(ap,msg);
    vsprintf(buffer,msg,ap);
    va_end(ap);

    print_error (buffer,ERR);

    if ( ext_error ) return 0; /* do not exit error routine is specified */
    
    exit (1);
}

int G_warning ( char *msg, ...)
{
    char buffer[2000];
    va_list ap;

    if (no_warn) return 0;

    va_start(ap,msg);
    vsprintf(buffer,msg,ap);
    va_end(ap);
    print_error (buffer,WARN);

    return 0;
}

int G_suppress_warnings (int flag)
{
    int prev;

    prev = no_warn;
    no_warn = flag;
    return prev;
}

int G_sleep_on_error (int flag)
{
    int prev;

    prev = !no_sleep;
    no_sleep = !flag;
    return prev;
}

int G_set_error_routine ( int (*error_routine)())
{
    ext_error = error_routine; /* Roger Bivand 17 June 2000 */
    return 0;
}

int G_unset_error_routine ()
{
    ext_error = 0; /* Roger Bivand 17 June 2000 */

    return 0;
}

/* Print info to stderr and optionaly to log file and optionaly send mail */
static int print_error(char *msg,int type)
{
    static char *prefix_std[3];
    int fatal, format;
    
    if ( !prefix_std[0] ) { /* First time: set prefixes  */
        prefix_std[0] = "";
	prefix_std[1] = _("WARNING: ");
	prefix_std[2] = _("ERROR: ");
    }
    
    if ( type == ERR )
	fatal = 1;
    else /* WARN */
	fatal = 0;

    if ( (type == WARN || type == ERR) && ext_error) { /* Function defined by application */
	ext_error (msg, fatal);
    } else {
	char *w;
	int len, lead;

        format = G_info_format();

	if ( format == G_INFO_FORMAT_STANDARD ) {
	    if ( type == WARN || type == ERR ) { 
		log_error (msg, fatal);
	    }

	    fprintf(stderr,"%s", prefix_std[type] );
	    len = lead = strlen ( prefix_std[type] );
	    w = msg;

	    while (print_word(stderr,&w,&len,lead))
		    ;

	    if (isatty(fileno(stderr))) { /* Bell */
		fprintf(stderr,"\7");
		fflush (stderr);
		if (!no_sleep)
		    sleep (5);
	    } else if ( (type == WARN || type == ERR) && getenv("GRASS_ERROR_MAIL")) { /* Mail */
		mail_msg (msg, fatal);
	    }
	} else { /* GUI */
	    print_sentence ( stderr, type, msg );
	}
    }

    return 0;
}

static int log_error ( char *msg,int fatal)
{
    FILE *pwd;
    char cwd[1024];
    long clock;
    char *home;
    char *gisbase;

/* get time */
    clock = time(0);

/* get current working directory */
    sprintf(cwd,"?");
    if ( (pwd = G_popen ("pwd","r")) )
    {
	if (fgets(cwd, sizeof cwd, pwd))
	{
	    char *c;

	    for (c = cwd; *c; c++)
		if (*c == '\n')
		    *c = 0;
	}
	G_pclose (pwd);
    }

/* write the 2 possible error log files */
    if( (gisbase = G_gisbase ()) )
	write_error (msg, fatal, gisbase, clock, cwd);
    home = G__home();
    if (home && gisbase && strcmp (home, gisbase))
	write_error (msg, fatal, home, clock, cwd);


    return 0;
}

static int write_error ( char *msg, int fatal, char *dir, long clock, char *cwd)
{
    char logfile[1000];
    FILE *log;
    char *ctime();

    if (dir == 0 || *dir == 0)
	return 1;
    sprintf (logfile, "%s/GIS_ERROR_LOG", dir) ;

/* logfile must exist and be writeable */
    if (access (logfile, 0) != 0)
	return 1;

    log = fopen (logfile,"a");
    if (!log)
	return 1;

    fprintf(log,"-------------------------------------\n");
    fprintf(log,"%-10s %s\n", "program:", G_program_name());
    fprintf(log,"%-10s %s\n", "user:", G_whoami());
    fprintf(log,"%-10s %s\n", "cwd:", cwd);
    fprintf(log,"%-10s %s\n", "date:", ctime(&clock));
    fprintf(log,"%-10s %s\n", fatal?"error:":"warning:", msg);
    fprintf(log,"-------------------------------------\n");

    fclose (log);

    return 0;
}

static int mail_msg ( char *msg,int fatal)
{
    FILE *mail;
    char command[64];
    char *user;

    user = G_whoami();
    if (user == 0 || *user == 0)
	return 1;

    sprintf (command, "mail '%s'", G_whoami());
    if ( (mail = G_popen (command, "w")) )
    {
	fprintf(mail,"GIS %s: %s\n",fatal?"ERROR":"WARNING",msg);
	G_pclose (mail);
    }

    return 0;
}

/* Print one word, new line if necessary */
static int print_word ( FILE *fd, char **word, int *len, int lead)
{
    int  wlen, start, totlen;
    int  nl;
    char *w,*b;

    start = *len;
    w = *word;

    nl = 0;
    while (*w == ' ' || *w == '\t' || *w == '\n')
	if(*w++ == '\n')
	    nl++;

    wlen = 0;
    for (b = w; *b != 0 && *b != ' ' && *b != '\t' && *b != '\n'; b++)
	wlen++;

    if (wlen == 0)
    {
	fprintf (fd, "\n");
	return 0;
    }

    if ( start > lead ) { /* add space */
	totlen = start + wlen + 1;
    } else {
	totlen = start + wlen; 
    }
    
    if ( nl != 0 || totlen > 75)
    {
	while (--nl > 0)
	    fprintf (fd, "\n");
	fprintf (fd, "\n%*s",lead,"");
	start = lead;
    }

    if ( start > lead ) {
      fprintf (fd, " ");
      start++;
    }

    *len = start + wlen;

    while (wlen-- > 0)
	fprintf (fd, "%c", *w++);

    *word = w;
    return 1;
}

/* Print one message, prefix inserted before each new line */
static void print_sentence ( FILE *fd, int type, char *msg )
{
    char *start;
    static char prefix[100];

    switch ( type ) {
	case MSG: 
    	    sprintf ( prefix, "GRASS_INFO_MESSAGE(%d,%d): ", (int)getpid(), message_id ); 
	    break;
	case WARN:
    	    sprintf ( prefix, "GRASS_INFO_WARNING(%d,%d): ", (int)getpid(), message_id ); 
	    break;
	case ERR:
    	    sprintf ( prefix, "GRASS_INFO_ERROR(%d,%d): ", (int)getpid(), message_id ); 
	    break;
    }

    start = msg;

    fprintf(stderr, "\n" );
    while ( *start != '\0' ) {
	fprintf ( fd, "%s", prefix);

	while ( *start != '\0' ) {
	    fprintf (fd, "%c", *start++);
		
	    if ( *start == '\n' ) {
	        *start++;
		break;
	    }
	}
	
	fprintf (fd, "\n" );
    }
    fprintf(stderr, "GRASS_INFO_END(%d,%d)\n", (int)getpid(), message_id );
    message_id++;
}
    
int G_info_format ( void ) 
{
    static int grass_info_format = -1;
    char    *fstr;
    
    if ( grass_info_format < 0) {
        fstr = getenv( "GRASS_MESSAGE_FORMAT" );

        if ( fstr && G_strcasecmp(fstr,"gui") == 0 )
	    grass_info_format = G_INFO_FORMAT_GUI;
        else
	    grass_info_format = G_INFO_FORMAT_STANDARD;
    }

    return grass_info_format;
}

