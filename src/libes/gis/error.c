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

#include "gis.h"

static int (*error)() = 0;
static int no_warn = 0;
static int no_sleep = 0;

extern char *getenv();


G_fatal_error (msg)
    char *msg;
{
    print_error (msg,1);
    exit (1);
}

G_warning (msg)
    char *msg;
{
    if (!no_warn)
	print_error (msg,0);
}

G_suppress_warnings (flag)
{
    int prev;

    prev = no_warn;
    no_warn = flag;
    return prev;
}

G_sleep_on_error (flag)
{
    int prev;

    prev = !no_sleep;
    no_sleep = !flag;
    return prev;
}

G_set_error_routine (error_routine)
    int (*error_routine)();
{
    error = error_routine;
}

G_unset_error_routine ()
{
    error = 0;
}

static print_error (msg, fatal)
    char *msg;
{
    static active = 0;	/* to avoid recursion */

    if (active)
    {
	fprintf(stderr,"%s: ",fatal?"ERROR":"WARNING");
	fprintf (stderr, "%s\n", msg);
	return;
    }
    active = 1;

    log_error (msg, fatal);

    if (error)
	error (msg, fatal);
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
}

static log_error (msg, fatal)
    char *msg;
{
    FILE *pwd;
    char cwd[1024];
    long clock;
    char *home;
    char *gisbase;

    long time();
    FILE *G_popen();

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

}

static write_error (msg, fatal, dir, clock, cwd)
    char *msg;
    char *dir;
    long clock;
    char *cwd;
{
    char logfile[1000];
    FILE *log;
    char *ctime();

    if (dir == 0 || *dir == 0)
	return;
    sprintf (logfile, "%s/GIS_ERROR_LOG", dir) ;

/* logfile must exist and be writeable */
    if (access (logfile, 0) != 0)
	return;

    log = fopen (logfile,"a");
    if (!log)
	return;

    fprintf(log,"-------------------------------------\n");
    fprintf(log,"%-10s %s\n", "program:", G_program_name());
    fprintf(log,"%-10s %s\n", "user:", G_whoami());
    fprintf(log,"%-10s %s\n", "cwd:", cwd);
    fprintf(log,"%-10s %s\n", "date:", ctime(&clock));
    fprintf(log,"%-10s %s\n", fatal?"error:":"warning:", msg);
    fprintf(log,"-------------------------------------\n");

    fclose (log);
}

static mail_msg (msg, fatal)
    char *msg;
{
    FILE *mail, *G_popen();
    char command[64];
    char *user;

    user = G_whoami();
    if (user == 0 || *user == 0)
	return;

    sprintf (command, "mail '%s'", G_whoami());
    if (mail = G_popen (command, "w"))
    {
	fprintf(mail,"GIS %s: %s\n",fatal?"ERROR":"WARNING",msg);
	G_pclose (mail);
    }
}

static
print_word (fd, word, len, lead)
    FILE *fd;
    char **word;
    int *len;
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
