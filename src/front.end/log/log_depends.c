#include <stdio.h>
#include <sys/types.h>
#include <sys/times.h>
#include <pwd.h>
#include "gis.h"

struct acct_info
{
    long     proctime;
    long     startdate;
    long     enddate;
    int  gid;
    int  uid;
    int  procid;
    char    *tty;
} acct_info;

char    _PROGNAME [BUFSIZ];
char    _LOGFILE [BUFSIZ];
char    *_usrnm;
struct   tms    tbuffer;

char    *ttyname();
char    *getlogin();



log_start ( prog, logfile)
	char    *prog, *logfile;
{
#ifdef FOO
    extern  char    *whoisuser();
    extern  char    _LOGFILE [];
    FILE     B;

    B._flag = _IOWRT + _IOSTRG ;

    B._ptr = _LOGFILE ;
    B._cnt = BUFSIZ - 1 ;

    va_start (ap);
    _doprnt (fmt, ap, &B) ;
    va_end (ap);
    putc ('\0', &B) ;

#endif /*FOO*/

	strcpy (_LOGFILE, logfile);
    acct_info.procid    = getpid ();    /* get process id   */
    acct_info.uid       = getuid ();    /* get user id      */
    acct_info.gid       = getgid ();    /* get group id     */
    acct_info.tty       = ttyname(0);
/*replace this with G_whoami()*/
    _usrnm      = getlogin(); /* modified for CAS 5/10/85 */
    strncpy (_PROGNAME, prog, 14) ;
    time (&acct_info.startdate);
}

log_quit ()
{
    static  int  flag = 0;
    extern  char     _LOGFILE [];
    FILE    *acct_fid;

    if (flag++)     /* a record has already been written    */
        return;

    time (&acct_info.enddate);
    times (&tbuffer);

    acct_info.proctime  = tbuffer.tms_utime
        + tbuffer.tms_stime;

    if ((acct_fid = fopen (_LOGFILE,"a")) == NULL) {
        perror(_LOGFILE);
        return;
    }

    if (fseek ( acct_fid, 0L, 2) == -1) {       /* append to the log */
        perror ( "quit: trying to seek to EOF");
        return (-1);
    }

    fprintf ( acct_fid,
        "%14.14s %5D.%0.2D ps %12.12s, %6D elapsed, %s,%9s\n",
        _PROGNAME,
        acct_info.proctime/60,
        ( (acct_info.proctime * 100) / 60) % 100,
        4 + ctime (&acct_info.startdate),
        acct_info.enddate - acct_info.startdate,
        acct_info.tty,
        _usrnm);
        /*
		  acct_info.gid & 0377,
        acct_info.procid);
		*/

    fclose ( acct_fid);
}

