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



log_write ( prog, logfile)
	char    *prog, *logfile;
{

    FILE    *acct_fid;

	strcpy (_LOGFILE, logfile);
    acct_info.procid    = getpid ();    /* get process id   */
    acct_info.uid       = getuid ();    /* get user id      */
    acct_info.gid       = getgid ();    /* get group id     */
    acct_info.tty       = ttyname(0);
    _usrnm      = G_whoami(); /* modified for CAS 5/10/85 */
    strncpy (_PROGNAME, prog, 14) ;

    time (&acct_info.startdate);

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
        "%-14.14s  %12.12s  %s,%9s\n",
        _PROGNAME,
        4 + ctime (&acct_info.startdate),
        acct_info.tty,
        _usrnm);

    fclose ( acct_fid);
}

