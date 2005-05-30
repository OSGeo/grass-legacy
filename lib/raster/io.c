#ifndef __MINGW32__

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#ifndef USE_G_SOCKS
#include <fcntl.h>
#include <pwd.h>
/* for locking based on inode number of a fifo */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <sys/stat.h>
#endif /* USE_G_SOCKS */

#include "graph.h"
#include "monitors.h"
#include "gis.h"
#include "raster.h"
#include "glocale.h"

#include "open.h"

#define BUFFERSIZ   2048

#define LOCK_OK 1
#define ALREADY_LOCKED 0
#define CANT_CREATE -1
#define CANT_READ -2
#define CANT_WRITE -3

#ifdef USE_G_SOCKS
static char *sockpath;
#else
static int _fifo_ino;
#endif

static unsigned char outbuf[BUFFERSIZ] ;
static int cursiz;
static volatile int no_mon;

static int _rfd;
static int _wfd;

static int quiet;

#ifndef USE_G_SOCKS
static int find_process (int);
static int get_ids (char *,int *,int);
static char *who_locked_driver();
static int lockfile(char *);
static int fifoto(char *,char *,int);
static int lock_driver (int);
#endif

static int unlock_driver (int);
static int sync_driver(char *);
static RETSIGTYPE dead(int);

int _get(char *,int);
int flushout (void);


int
_send_ident(int anint)
{
    unsigned char achar ;
    achar = anint;

    if( (cursiz+2) >= BUFFERSIZ)
        flushout() ;
    outbuf[cursiz++] = COMMAND_ESC ;
    outbuf[cursiz++] = achar ;

    return 0;
}

int
_send_char(unsigned char *achar)
{
    if( (cursiz+2) >= BUFFERSIZ)
        flushout() ;
    outbuf[cursiz++] = *achar ;
    if (*achar == COMMAND_ESC)
        outbuf[cursiz++] = 0 ;

    return 0;
}

int
_send_char_array(int num, unsigned char *achar)
{
    while (num-- > 0)
        _send_char (achar++);

    return 0;
}

int
_send_int_array(int num, int *anint)
{
    return _send_char_array(num * sizeof(int), (unsigned char *)anint) ;
}

int
_send_float_array(int num, float *afloat)
{
    return _send_char_array(num * sizeof(float), (unsigned char *)afloat) ;
}

int
_send_int(int *anint)
{
    return _send_char_array(sizeof(int), (unsigned char *)anint) ;
}

int
_send_float(float *afloat)
{
    return _send_char_array(sizeof(float), (unsigned char *)afloat) ;
}

int
_send_text(char *text)
{
    return _send_char_array(1 + strlen(text), (unsigned char *)text) ;
}

int
_get_char(char *achar)
{
    flushout() ;
    _get (achar, 1);

    return 0;
}

int
_get_int(int *anint)
{
    flushout() ;
    _get( (char *)anint, sizeof(int));

    return 0;
}

int
_get_float(float *afloat)
{
    flushout() ;
    _get( (char *)afloat, sizeof(float));

    return 0;
}

int
_get_text (char *buf)
{
    char *b;

    b = buf;
    do
        _get_char (b);
    while (*b++ != 0);

    return 0;
}

char *
_get_text_2 (void)
{
    static char *buf;
    static int len;
    int i;

    for (i = 0; ; i++)
    {
	if (i >= len)
	{
	    len += 1000;
	    buf = realloc(buf, len);
	    if (!buf)
	    {
		fprintf(stderr, _("Unable to allocate memory\n"));
		exit(1);
	    }
	}
        _get_char (&buf[i]);
	if (!buf[i])
	    break;
    }

    return buf;
}

int
_get (char *buf, int n)
{
    int x;
    while (n > 0)
    {
        x = read (_rfd, buf, n);
        if (x <= 0)
        {
            fprintf(stderr, _("ERROR %s from graphics driver.\n"),
		    x ? "reading" : "eof");
            exit(1);
        }
        n -= x;
        buf += x;
    }

    return 0;
}

int
flushout(void)
{
    if (cursiz)
    {
        write (_wfd, outbuf, (size_t) cursiz);
        cursiz = 0 ;
    }

    return 0;
}

/* R_open_driver for communication over fifos -- 7 Oct 87 */
/* In verbose mode, errors print a message and exit.  In quiet mode, errors */
/* return a code and no messages are printed.  The flag quiet is set */
/* by calling R__open_quiet just before calling R_open_driver. */
/* Returns in quiet mode opens are defined in open.h */


/*!
 * \brief initialize graphics
 *
 * Initializes connection to
 * current graphics driver. Refer to GRASS User's Manual entries on the
 * <i>d.mon</i> command. If connection cannot be made, the application module
 * sends a message to the user stating that a driver has not been selected or
 * could not be opened. Note that only one application module can be connected to
 * a graphics driver at once.
 * After all graphics have been completed, the driver should be closed.
 *
 *  \param void
 *  \return int
 */

int
R_open_driver(void)
{
    int verbose;
    int try;
    struct MON_CAP *mon;
    char *name;
#ifndef USE_G_SOCKS
    char *user;
    char our_input_file[512], our_output_file[512];
    int key, lock;
    char *key_string;
    struct stat stat_buf;
#endif

    verbose = !quiet;
    quiet = 0;

    name = getenv("MONITOR_OVERRIDE");
    if (!name)
	name = G__getenv("MONITOR");

    if (!name)
    {
        if (verbose)           /* #31 Aug 87 - want error stuff */
        {
            fprintf(stderr,_("No graphics monitor has been selected for output.\n"));
            fprintf(stderr,_("Please run \"d.mon\" to select a graphics monitor.\n"));
            exit(-1);
        }
        return(NO_MON);
    }

    if ((mon = R_parse_monitorcap(MON_NAME,name)) == NULL)
    {
        if (verbose)
        {
            fprintf(stderr,_("No such graphics monitor as <%s>.\n"),name);
            fprintf(stderr,_("Please run \"d.mon\" to select a valid graphics monitor.\n"));
            exit(-1);
        }
        return(NO_MON);
    }

#ifdef USE_G_SOCKS
    /* Get the full path to the unix socket */
    if ( (sockpath = G_sock_get_fname (name)) == NULL)
    {
        if (verbose)
        {
            fprintf (stderr, _("Failed to get socket name for monitor <%s>.\n"),
                            name);
        }
        return (NO_MON);
    }

    /* See if the socket exists, if it doesn't no point in trying to
     * connect to it.
     */
    if (!G_sock_exists (sockpath))
    {
        if (verbose)
        {
            fprintf (stderr, _("No socket to connect to for monitor <%s>.\n"),
                            name);
        }
        return (NO_MON);
    }

    /** Used to be a bunch of stuff about locking here.  This is not
     * necessary with sockets, since the server will only accept
     * one connection at a time, all other connections will be refused
     * until it closes it's current connection.
     */

    /** We try to make a connection now **/
    for (try = 0; try < 2; try++)
    {
        _wfd = G_sock_connect (sockpath);
        if (_wfd > 0) /* success */
        {
            _rfd = dup(_wfd);
            sync_driver (name);
            return (OK);
        }
        switch (errno)
        {
            case ECONNREFUSED:
            case EADDRINUSE:
                    if (verbose)
                        fprintf (stderr, _("Socket is already in use or not "
				 "accepting connections.\n"
				 "Use d.mon to select a monitor\n"));
                    return (NO_RUN);
                    break;
            case EBADF:
            case ENOTSOCK:
                    if (verbose)
                        fprintf (stderr, _("Trying to connect to something "
				 "not a socket.\nProbably program "
				 "error.\n"));
                    return (NO_RUN);
                    break;
            case ETIMEDOUT:
                    if (verbose)
                        fprintf (stderr, _("Connect attempt timed out. "
				 "Probably an error with the server.\n"));
                    return (NO_RUN);
                    break;
            default:
                    break;
        }
        fprintf (stderr, _("Not connected...\n"));
        if (verbose && try < 1)
        {
            fprintf (stderr, _("Couldn't connect to monitor. "
		     "Will try once more.\n"));
            sleep (1);
        }
        else if (verbose && try > 0)
        {
            fprintf (stderr, _("Connection failed.\n"));
        }
    }
            
    /* We couldn't connect... */
    return (NO_RUN);

#else /* USE_G_SOCKS */

    /* get the fifos and get the inode number of one of them */
    sscanf(mon->link,"%s %s",our_output_file,our_input_file);
    if (stat (our_output_file, &stat_buf) != 0)
    {
        if (verbose)
        {
            fprintf (stderr, _("Can't stat %s\n"), our_output_file);
            exit(-1);
        }
        return (LOCK_FAILED);
    }
    _fifo_ino = stat_buf.st_ino; /* global: used by lockfile() */

    key_string = getenv("GIS_LOCK");
    if (key_string == NULL || sscanf(key_string,"%d",&key) != 1 || key <= 0)
        key = 0;
    lock = lock_driver(key);
    if (lock == 0)
    {
        if (verbose)
        {
            if ((user = who_locked_driver()) == NULL)
                fprintf(stderr,_("Error - Monitor <%s> is in use.\n"),name);
            else
                fprintf(stderr,_("Error - Monitor <%s> is in use by %s.\n"),name,user);
            exit(-1);
        }
        return(LOCKED);
    }
    if (lock < 0)
    {
        if (verbose)
        {
            char file[512];
            fprintf(stderr,_("Error - Could not complete locking process for monitor <%s>.\n"),name);
            lockfile(file);
            fprintf (stderr, "Lock file is %s\n", file);
            exit(-1);
        }
        return(LOCK_FAILED);
    }
    if (verbose)
    {
        for (try = 0; try < 2; try++)
        {
            switch (fifoto (our_input_file,our_output_file,try?15:3))
            {
            case -1:
                fprintf(stderr, _("Error - Can't set up pipe to graphics device.\n"));
                unlock_driver(1);
                exit(-1);
            case 0:
                if (try)
                {
                    fprintf (stderr, _("Error - Graphics monitor <%s> not running!\n"),name);
                    unlock_driver(1);
                    exit(1);
                }
                fprintf (stderr, _("Please start graphics monitor <%s>.\n"),name);
                break;
            default:
                sync_driver(name); /* syncronize driver */
                return(0);
            }             /* switch */
        }               /* for */
    }
    else /* non-verbose mode */
    {
    /*  switch (fifoto(our_input_file,our_output_file,3)) */
        switch (fifoto(our_input_file,our_output_file,1))
        {
        case -1:
            unlock_driver(1);
            return(NO_OPEN);
        case 0:
            unlock_driver(1);
            return(NO_RUN);
        default:
            return(OK);
        }
    }

    return 0;
#endif /* USE_G_SOCKS */
}

int
R__open_quiet(void)
{
    quiet = 1;

    return 0;
}

static int
sync_driver(char *name)
{
    RETSIGTYPE (*sigalarm)(int);
    int try;
    int count;
    unsigned char c;

    _send_ident (BEGIN);
    flushout();

/*
 * look for at least BEGIN_SYNC_COUNT zero bytes
 * then look for COMMAND_ESC
 *
 * try twice. first timeout is warning, second is fatal
 */
    count = 0;
    sigalarm = signal(SIGALRM, dead);
    for (try = 0; try < 2; try++)
    {
        no_mon = 0;
        alarm(try?10:5);
        while(no_mon == 0)
        {
            if (read (_rfd, &c, (size_t) 1) != 1)
            {
                if (no_mon)
                    break; /* from while */
                fprintf (stderr, _("ERROR - eof from graphics monitor.\n"));
                exit(-1);
            }
            if (c == 0)
                count++;
            else if (c == COMMAND_ESC && count >= BEGIN_SYNC_COUNT)
                break;
            else
                count = 0;  /* start over */
        }
        alarm(0);
        signal(SIGALRM, sigalarm);
        if (no_mon == 0)
            return 1;   /* ok! */
        if (try)
            break;

        fprintf (stderr, _("Warning - no response from graphics monitor <%s>.\n"),
            name);
        fprintf (stderr, _("Check to see if the mouse is still active.\n"));
        signal(SIGALRM, dead);
    }
    fprintf (stderr, _("ERROR - no response from graphics monitor <%s>.\n"),
        name);
    exit(-1);
}

static RETSIGTYPE
dead(int sig)
{
    no_mon = 1 ;
}

int
_hold_signals (int hold)
{
    static RETSIGTYPE (*sigint)(int);
    static RETSIGTYPE (*sigquit)(int);

    if (hold)
    {
        sigint  = signal (SIGINT, SIG_IGN);
        sigquit = signal (SIGQUIT, SIG_IGN);
    }
    else
    {
        signal (SIGINT, sigint);
        signal (SIGQUIT, sigquit);
    }

    return 0;
}

#ifdef USE_G_SOCKS

static int
unlock_driver (int wipeout)
{
    return -1;
}

#else /* USE_G_SOCKS */

/******************************************************************
* lock_driver (pid)
*
*   this routine "locks" the driver for process pid as follows:
*
*   1. if lockfile exists, 2 old pids are read out of the file.
*
*      the first pid is the global lock, the second is the
*      process id lock. If both match the pid and the current
*      pid, nothing needs to be done.
*
*   2. If the 2nd pid is not the same as the current process
*      and that process is still running, then can't lock
*
*   3. If the first pid is not the same and that process is still
*      running, then can't lock
*
*   4. the driver is locked by writing pid and curretn pid into the file.
*
*   5. The user's name is also written to the file
*
* note: since file is created by this routine, it shouldn't be
*       a file that is used for any other purpose.
*
*       also, this lock mechanism is advisory. programs must call this
*       routine and check the return status.
*
* returns:
*       1 ok lock is in place.
*       0 could not lock because another process has the file locked already
*      -1 error. could not create the lock file
*      -2 error. could not read the lock file.
*      -3 error. could not write the lock file.
******************************************************************/

static int
lockfile(char *file)
{
    char *name;
    char *hostname ;
    int mask;
    char lock_dir[1024] ;

/* create the lock_dir */
    mask = umask(0);
    sprintf (lock_dir, "%s/locks", G_gisbase());
    mkdir (lock_dir,01777);

/* get machine name, if it has one */
    hostname = G__machine_name();
    if (hostname)
    {
        for(name=hostname; *name!='\0'; name++) /* use only first part */
        {
            if (*name == '.')
            {
                *name = 0 ;
                break ;
            }
        }
        strcat (lock_dir, "/");
        strcat (lock_dir, hostname);
        mkdir (lock_dir,0777);
    }
    umask (mask);

    sprintf (file, "%s/mon.%d", lock_dir, _fifo_ino);

        return 0;
}

static int
lock_driver (int lock_pid)
{
    char file[512];
    int fd;
    int mask;
    int n;
    int id[3];
    int me;

    lockfile(file);
    me = getpid();
    if (access (file, 0) == 0)      /* file exists */
    {
        for (n = 0; n < 2; n++)
        {
            if (get_ids (file, id, 2))
                break;
            if (n == 0)
                sleep(1); /* allow time for file creator to write its pid */
        }
        if (n == 2)
            return CANT_READ;
        if (lock_pid == id[1] && me == id[0])
            return LOCK_OK;
        if (me != id[0] && id[0] >= 0 && find_process (id[0]))
            return ALREADY_LOCKED;
        if (lock_pid != id[1] && find_process (id[1]))
            return ALREADY_LOCKED;
    }
    mask = umask (0);
    fd = creat (file, 0666) ;
    umask (mask);
    if (fd < 0)
        return CANT_CREATE;
    id[0] = me;
    id[1] = lock_pid;
    id[2] = getuid();
    if (write(fd, id, sizeof id) != sizeof id)
    {
        close (fd);
        return CANT_WRITE;
    }
    close (fd);
    return LOCK_OK;
}

static char *
who_locked_driver(void)
{
    char file[512];
    int id[3];
    struct passwd *pw;

    lockfile (file);
    if (!get_ids (file, id, 3))
        return ((char *)NULL);
    if ((pw = getpwuid(id[2])) == NULL)
        return ((char *)NULL);
    return (pw->pw_name);
}

static int
get_ids (char *file, int *id, int x)
{
    int fd;
    int n;

    if ((fd = open (file, 0)) < 0)
        return 0;
    n = read (fd, id, x*sizeof (*id));
    close (fd);
    return (n == x*sizeof (*id));
}

static int
find_process (int pid)
{
/* attempt to kill pid with NULL signal. if success, then
   process pid is still running. otherwise, must check if
   kill failed because no such process, or because user is
   not owner of process
*/
    if (pid <= 0)
        return 0;/* no such process */

    if (kill (pid, 0) == 0)
        return 1;
    return errno != ESRCH;
}

static int
unlock_driver (int wipeout)
{
    char file[512];
    int fd;
    int pid;

    lockfile(file);
    if (*file == 0) return 0;
/*
 * two flavors of unlock.
 *   wipeout==0 for current process only
 *   wipeout==1 for global locking key as well
 */
    if (access (file,0) != 0)
        return 0;
    if (!wipeout)
    {
        fd = open (file, 1);
        if (fd < 0) wipeout = 1;
    }
    if (!wipeout)
    {
        pid = -1;
        if(write (fd, &pid, sizeof(pid)) != sizeof(pid))
            wipeout = 1;
        close (fd);
    }
    if (wipeout)
    {
        remove (file);
        if (access (file,0) != 0)
            return 1;
    }
    return -1;
}

/*************************************************
* fifoto(alarm_time)
*      this is the plumbing, the idea is to
*      open fifo pipes for read/write.
*************************************************/

static int
fifoto( char *input, char *output, int alarm_time)
{
    struct sigaction mysig, savesig;
    sigset_t mask;
    
    /* Set up signal handling */
    sigemptyset (&mask);
    mysig.sa_handler = dead;
    mysig.sa_mask = mask;
    mysig.sa_flags = 0;
    
    no_mon = 0;
    sigaction (SIGALRM, &mysig, &savesig);
    alarm(alarm_time);
    _wfd = open (output, O_WRONLY);
    alarm(0);
    sigaction (SIGALRM, &savesig, NULL);
    if (no_mon)
        return 0 ;

    no_mon = 0;
    sigaction (SIGALRM, &mysig, &savesig);
    alarm(alarm_time);
    _rfd = open (input, O_RDONLY);
    alarm(0);
    sigaction (SIGALRM, &savesig, NULL);
    if (no_mon)
        return 0 ;

    if( (_wfd == -1) || (_rfd == -1) )
        return -1;

    return 1 ;
}

#endif /* USE_G_SOCKS */

int
R_kill_driver(void)
{
    _send_ident(GRAPH_CLOSE);
    flushout();
    R_release_driver();

    return 0;
}


/*!
 * \brief terminate graphics
 *
 * This routine breaks the connection with the graphics driver opened by R_open_driver().
 *
 *  \param void
 *  \return int
 */

int
R_close_driver(void)
{
    R_stabilize();

    close (_rfd);
    close (_wfd);
    _wfd = _rfd = -1;
    unlock_driver(0);

    return 0;
}

int
R_release_driver(void)
{
    close (_rfd);
    close (_wfd);
    _wfd = _rfd = -1;
    unlock_driver (1);

    return 0;
}


/*!
 * \brief synchronize graphics
 *
 * Send all pending graphics commands to the graphics driver and cause 
 * all pending graphics to be drawn (provided the driver is written to 
 * comply).  This routine does more than <i>R_flush()</i> and in many 
 * instances is the more appropriate routine of the two to use.
 *
 *  \param void
 *  \return int
 */

int
R_stabilize(void)
{
    char c;

    flushout();
    _send_ident (RESPOND);
    _get_char (&c);

    return 0;
}

#else /* __MINGW32__ */


#endif /* __MINGW32__ */
