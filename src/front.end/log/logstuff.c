
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<unistd.h>
#include	<sys/types.h>
#include	<sys/times.h>
#include 	<varargs.h>
#include 	<pwd.h>
#include 	"defines.h"

#define _LOGFILE      "/usr/export/home/zorro/stigberg/log_test"

#ifndef BUFSIZ
#define BUFSIZ          1024
#endif BUFSIZ

#ifndef ADMINISTRATOR
#define ADMINISTRATOR   "casper"
#endif  ADMINISTRATOR



#define INFINITY        0x7fffffff
#define DIRMODE         0700
#define FILEMODE        0600

#define PERIODS         5

log_start ( "CAS", "%s/%s", CAS, LOGFILE);

/*
 * Name:	log_start()
 *		quit()
 *
 * Function:	To write a record of the user's usage to a log.
 *
 * Synopsis:	log_start(prog, logfile)
 *		char *prog, *logfile;
 *
 *		quit()
 *
 * Modified:	Calvin Corbin	30 July 1979
 *		Alan Edwards 	 1 Aug  1983
 */


struct acct_info
{
	long	 proctime;
	long	 startdate;
	long	 enddate;
	int	 gid;
	int	 uid;
	int	 procid;
	char	*tty;
} acct_info;

char	_PROGNAME [BUFSIZ];
char	_LOGFILE [BUFSIZ];
char	*_usrnm;
struct	 tms	tbuffer;

log_start ( prog, fmt, va_alist)
register	char	*prog, *fmt;
va_dcl
{
	va_list ap;
	extern	char	*whoisuser();
	extern	char	 _LOGFILE [];
	FILE	 B;

	B._flag = _IOWRT + _IOSTRG ;
	B._ptr = _LOGFILE ;
	B._cnt = BUFSIZ - 1 ;

	va_start (ap);
	_doprnt (fmt, ap, &B) ;
	va_end (ap);
	putc ('\0', &B) ;

	acct_info.procid	= getpid ();	/* get process id	*/
	acct_info.uid		= getuid ();	/* get user id		*/
	acct_info.gid		= getgid ();	/* get group id		*/
	acct_info.tty		= ttyname(0);
	_usrnm		= whoisuser(); /* modified for CAS 5/10/85 */
	strncpy (_PROGNAME, prog, 14) ;
	time (&acct_info.startdate);
}

quit ()
{
	static	int	 flag = 0;
	extern	char	 _LOGFILE [];
	FILE	*acct_fid;

	if (flag++)		/* a record has already been written	*/
		return;

	time (&acct_info.enddate);
	times (&tbuffer);

	acct_info.proctime	= tbuffer.tms_utime
	    + tbuffer.tms_stime;

	if ((acct_fid = fopen (_LOGFILE,"a")) == NULL) {
		perror(_LOGFILE);
		return;
	}

	if (fseek ( acct_fid, 0L, 2) == -1) {		/* append to the log */
		perror ( "quit: trying to seek to EOF");
		return (-1);
	}

	fprintf ( acct_fid,
	    "%14.14s %5D.%0.2D ps, %12.12s, %6D elapsed, %s,%12s, %3dgid, %5dprocid\n",
	    _PROGNAME,
	    acct_info.proctime/60,
	    ( (acct_info.proctime * 100) / 60) % 100,
	    4 + ctime (&acct_info.startdate),
	    acct_info.enddate - acct_info.startdate,
	    acct_info.tty,
	    _usrnm,
	    acct_info.gid & 0377,
	    acct_info.procid);

	fclose ( acct_fid);
}


/*
 * whoisuser - identify user by name
 *
 * returns a pointer to a 'safe' (non-static) string identifying
 * the user, obtained from getlogin(), getpwuid(), or whatever.
 *
 */


char *
whoisuser ()
{
	char *name;
	char *ename;
	char *lname;
	char *sname = ADMINISTRATOR;
	struct passwd *pw;

	if (! (lname = getlogin ()) || !*lname)	/* Get the login name	*/
		if (! (pw = getpwuid (getuid ())))
			lname = "login name unknown?";
		else
			lname = pw->pw_name;

	if ((ename = getenv ("USER")))	/* Get environment name	*/
		if (strcmp(sname,lname))
			name = lname;
		else
			name = ename;
	else name = lname;

	return (strcpy (malloc (strlen (name) + 1), name));
}
