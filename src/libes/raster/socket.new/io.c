#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include "graph.h"
#include "monitors.h"
#include "gis.h"

/* for locking based on inode number of a fifo */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <sys/stat.h>

#define BUFFERSIZ   2048

extern int errno;

static int _rfd;
static int _wfd;
static unsigned char outbuf[BUFFERSIZ], inbuf[BUFFERSIZ] ;
static char *sockpath ;
static int cursiz = 0 ;
static int n_read = 0 ;
static int atbuf = 0 ;
static int no_mon ;

static int sync_driver(char *);
static void dead(int);
static void (*sigalarm)();
static void (*sigint)();
static void (*sigquit)();
static int _get(char *,int);
static int _rec (char *);


int
_send_ident(anint)
    int anint ;
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
_send_char (achar)
    unsigned char *achar ;
{
    if( (cursiz+2) >= BUFFERSIZ)
        flushout() ;
    outbuf[cursiz++] = *achar ;
    if (*achar == COMMAND_ESC)
        outbuf[cursiz++] = 0 ;

    return 0;
}

int
_send_char_array(num, achar)
    register int num ;
    register unsigned char *achar ;
{
    while (num-- > 0)
        _send_char (achar++);

    return 0;
}

int
_send_int_array(num, anint)
    int num ;
    int *anint ;
{
    _send_char_array(num * sizeof(int), (unsigned char *)anint) ;

    return 0;
}

int
_send_float_array(num, afloat)
    int num ;
    float *afloat ;
{
    _send_char_array(num * sizeof(float), (unsigned char *)afloat) ;

    return 0;
}

int
_send_int(anint)
    int *anint ;
{
    _send_char_array(sizeof(int), (unsigned char *)anint) ;

    return 0;
}

int
_send_float(afloat)
    float *afloat ;
{
    _send_char_array(sizeof(float), (unsigned char *)afloat) ;

    return 0;
}

int
_send_text(text)
    char *text ;
{
    _send_char_array(1 + strlen(text), (unsigned char *)text) ;

    return 0;
}

int
_get_char(achar)
    char *achar ;
{
    flushout() ;
    _get (achar, 1);

    return 0;
}

int
_get_int(anint)
    int *anint ;
{
    flushout() ;
    _get( (char *)anint, sizeof(int));

    return 0;
}

int
_get_float(afloat)
    float *afloat ;
{
    flushout() ;
    _get( (char *)afloat, sizeof(float));

    return 0;
}

int
_get_text (buf)
    char *buf;
{
    char *b;

    b = buf;
    do
	_get_char (b);
    while (*b++ != 0);

    return 0;
}

static int
_get(buf, n)
char *buf;
int n;
{
    int stat;
    while (n-- > 0) _rec(buf++);

    return 0;
}


static int
_rec (buf)
    char *buf;
{
    if (atbuf == n_read)
    {
	atbuf = 0;
        n_read = read (_rfd, &outbuf, sizeof outbuf);
    }
    *buf = outbuf[atbuf++];

    return 0;
}

int
flushout(void)
{
    if (cursiz)
    {
        write (_wfd, &outbuf, (size_t) cursiz);
        cursiz = 0 ;
    }

    return 0;
}



/* R_open_driver for communication over fifos -- 7 Oct 87 */
/* In verbose mode, errors print a message and exit.  In quiet mode, errors */
/* return a code and no messages are printed.  The flag quiet is set */
/* by calling R__open_quiet just before calling R_open_driver. */
/* Returns in quiet mode opens are defined in open.h */

#include "open.h"
static int quiet = 0;        /* #9 Sep 87 */


int
R_open_driver()
{
    int verbose;
    int try;
    struct MON_CAP *mon, *R_parse_monitorcap();
    char *name, *G__getenv();

    verbose = !quiet;
    quiet = 0;
    if ((name = G__getenv("MONITOR")) == NULL)
    {
        if (verbose)           /* #31 Aug 87 - want error stuff */
        {
            fprintf(stderr,"No graphics monitor has been selected for output.\n");
            fprintf(stderr,"Please run \"d.mon\" to select a graphics monitor.\n");
            exit(-1);
        }
        return(NO_MON);
    }

    if ((mon = R_parse_monitorcap(MON_NAME,name)) == NULL)
    {
        if (verbose)
        {
            fprintf(stderr,"No such graphics monitor as <%s>.\n",name);
            fprintf(stderr,"Please run \"d.mon\" to select a valid graphics monitor.\n");
            exit(-1);
        }
        return(NO_MON);
    }

    /* Get the full path to the unix socket */
    if ( (sockpath = G_sock_get_fname (name)) == NULL)
    {
        if (verbose)
	{
	    fprintf (stderr, "Failed to get socket name for monitor <%s>.\n",
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
	    fprintf (stderr, "No socket to connect to for monitor <%s>.\n",
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
            _rfd = _wfd;
            sync_driver (name);
            return (OK);
        }
	switch (errno)
	{
	    case ECONNREFUSED:
	    case EADDRINUSE:
		    if (verbose)
			fprintf (stderr, "Socket is already in use or not "\
					"accepting connections.\n"\
                                        "Use d.mon to select a monitor\n");
                    return (NO_RUN);
	            break;
	    case EBADF:
	    case ENOTSOCK:
		    if (verbose)
			fprintf (stderr, "Trying to connect to something "\
					"not a socket.\nProbably program "\
					"error.\n");
		    return (NO_RUN);
		    break;
	    case ETIMEDOUT:
		    if (verbose)
			fprintf (stderr, "Connect attempt timed out. "\
					"Probably an error with the server.\n");
		    return (NO_RUN);
		    break;
	    default:
		    break;
	}
        fprintf (stderr, "Not connected...\n");
        if (verbose && try < 1)
        {
            fprintf (stderr, "Couldn't connect to monitor. "\
                             "Will try once more.\n");
            sleep (1);
        }
        else if (verbose && try > 0)
        {
            fprintf (stderr, "Connection failed.\n");
        }
    }
            
    /* We couldn't connect... */
    return (NO_RUN);
}


int
R__open_quiet()
{
    quiet = 1;

    return 0;
}

static int
sync_driver(name)
    char *name;
{
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
                fprintf (stderr, "ERROR - eof from graphics monitor.\n");
                exit(-1);
            }
/*	    c = cb.c[0]; */
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

        fprintf (stderr, "\7Warning - no response from graphics monitor <%s>.\n",
            name);
        fprintf (stderr, "Check to see if the mouse is still active.\n");
        signal(SIGALRM, dead);
    }
    fprintf (stderr, "ERROR - no response from graphics monitor <%s>.\n",
        name);
    exit(-1);
}

static void
dead(int dummy)
{
    no_mon = 1 ;
}

int _hold_signals (int hold)
{
    if (hold)
    {
        sigint  = (void (*)()) signal (SIGINT, SIG_IGN);
        sigquit = (void (*)()) signal (SIGQUIT, SIG_IGN);
    }
    else
    {
        signal (SIGINT, sigint);
        signal (SIGQUIT, sigquit);
    }

	return 0;
}


int
R_kill_driver()             /* #31 Aug 87 - stop a driver */
{
    _send_ident(GRAPH_CLOSE);       /* #31 Aug 87 - tell driver to exit */
    flushout();
    R_release_driver();

    return 0;
}

int
R_close_driver()
{
    R_stabilize();
    close (_wfd);
    _wfd = _rfd = -1;
    return 0;
}

int
R_release_driver()
{
    close (_wfd);
    _wfd = _rfd = -1;
    return 0;
}

int
R_stabilize()
{
    char c;

    flushout();
    _send_ident (RESPOND);
    _get_char (&c);

    return 0;
}

/* vim: set softtabstop=4 shiftwidth=4 expandtab : */
