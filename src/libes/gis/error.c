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

#include <string.h>
#include <unistd.h>
#include <time.h>
#include "gis.h"

#include <stdarg.h>

#ifdef sun
#include <varargs.h>
#endif

/* static int (*error)() = 0; */
static int (*ext_error)() = 0; /* Roger Bivand 17 June 2000 */
static int no_warn = 0;
static int no_sleep = 1;

extern char *getenv();
static int print_word (FILE *,char **,int *,int);
static int print_error(char *,int);
static int mail_msg (char *,int);
static int write_error(char *, int,char *,long,char *);
static int log_error (char *,int);

int G_fatal_error ( char *msg,...)
{
    char buffer[256];  /* No novels to the error logs, OK? */
    va_list ap;

    va_start(ap,msg);
    vsprintf(buffer,msg,ap);
    va_end(ap);
    print_error (buffer,1);

    exit (1);
}

int G_warning ( char *msg, ...)
{
    char buffer[256];
    va_list ap;

    if (no_warn) return 0;

    va_start(ap,msg);
    vsprintf(buffer,msg,ap);
    va_end(ap);
    print_error (buffer,0);

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
    /* error = error_routine; */
    ext_error = error_routine; /* Roger Bivand 17 June 2000 */
    return 0;
}

int G_unset_error_routine ()
{
    /* error = 0; */
    ext_error = 0; /* Roger Bivand 17 June 2000 */

    return 0;
}

static int print_error(char *msg,int fatal)
{
    static int active = 0;	/* to avoid recursion */

    if (active)
    {
	/* fprintf(stderr,"%s: ",fatal?"ERROR":"WARNING");
	fprintf (stderr, "%s\n", msg);
	return -1; */
	if (ext_error) { /* Roger Bivand 18 June 2000 */
	    ext_error(msg, fatal);
	    return -1;
	}
	else {
	    fprintf(stderr,"%s: ",fatal?"ERROR":"WARNING");
	    fprintf (stderr, "%s\n", msg);
	    return -1;
	}
    }
    active = 1;

    log_error (msg, fatal);

    /* if (error)
	error (msg, fatal); */
    if (ext_error) /* Roger Bivand 17 June 2000 */
	ext_error (msg, fatal);
    else
    {
	char *w;
	int len, lead;

	fprintf(stderr,"%s:",fatal?"ERROR":"WARNING");
	len = lead = strlen (fatal?"ERROR":"WARNING")+1;
	w = msg;
	while (print_word(stderr,&w,&len,lead))
		;
	if (isatty(fileno(stderr)))
	{
	    fprintf(stderr,"\7");
	    fflush (stderr);
	    if (!no_sleep)
		sleep (5);
	}
	else if (!getenv("GRASS_STDERR"))
	    mail_msg (msg, fatal);
    }
    active = 0;

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
    if (pwd = G_popen ("pwd","r"))
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
    if(gisbase = G_gisbase ())
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
    if (mail = G_popen (command, "w"))
    {
	fprintf(mail,"GIS %s: %s\n",fatal?"ERROR":"WARNING",msg);
	G_pclose (mail);
    }

    return 0;
}

static int print_word ( FILE *fd, char **word, int *len,int lead)
{
    int i,n;
    int nl;
    char *w,*b;

    n = *len;
    w = *word;

    nl = 0;
    while (*w == ' ' || *w == '\t' || *w == '\n')
	if(*w++ == '\n')
	    nl++;
    i = 0;
    for (b = w; *b != 0 && *b != ' ' && *b != '\t' && *b != '\n'; b++)
	i++;
    if (i == 0)
    {
	fprintf (fd, "\n");
	return 0;
    }
    n += i + 1;
    if (nl != 0 || n > 75)
    {
	while (--nl > 0)
	    fprintf (fd, "\n");
	fprintf (fd, "\n%*s",lead,"");
	n = lead + 1;
    }
    fprintf (fd, " ");
    while (i-- > 0)
	fprintf (fd, "%c", *w++);
    *len = n;
    *word = w;
    return 1;
}
