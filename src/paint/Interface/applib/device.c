#include <stdio.h>
#include <signal.h>

static int rfd;
static int wfd;
static int xchild;
static char *xpgm ;
static unsigned char xbuf[512];
static int xcount ;

P__closedev ()
{
    int status;
    int pid;

    P__flushdev ();
    close (rfd);
    close (wfd);	/* child will read EOF */
    while ((pid = wait(&status)) > 0 && pid != xchild)
	    ;
    return (pid > 0 ? 1 : 0);
}

P__errordev (err)
    char *err;
{
    fprintf (stderr, "ERROR %s ", err);
    if (xpgm != NULL)
	fprintf (stderr, "%s ", xpgm);
    fprintf (stderr, "driver\n");
    exit(1);
}

P__flushdev ()
{
    if(xcount > 0 && write (wfd, xbuf, xcount) != xcount)
	P__errordev ("error writing to");
    xcount = 0;
}

/*************************************************
 * P__opendev (pgm, argv, painter)
 *	start up a child process and open a two way pipe to/from child
 *	child will read/write stdin/stdout
 * note: can not tell if the exec of pgm succeeded.
 *       the first writedev should fail in this case
 *************************************************/


#define READ  0
#define WRITE 1

P__opendev (pgm, argv, name)
    char *pgm;
    char *argv[];
    char *name;
{
    int p1[2], p2[2], pid;
    char *malloc();
    int i,j;

/* save the program name */
    xpgm = malloc (strlen(name) + 1);
    if (xpgm != NULL)
	strcpy (xpgm, name);

/* open the pipes */
    if ((pipe(p1) < 0 ) || (pipe(p2) < 0 )) 
	P__errordev ("can't open any pipes to");

/* create a child */
    if ((pid = fork()) < 0) 
	P__errordev ("can't create fork to");

    if (pid > 0) 	/* parent */
    {
	close(p1[READ]);
	close(p2[WRITE]);

	rfd = p2[READ];
	wfd = p1[WRITE];
	xchild = pid;
	xcount = 0;
    } 
    else 	/* child process */
    {
	close(p1[WRITE]);
	close(p2[READ]);

	close (0);
	close (1);

	if (dup(p1[READ]) != 0) 
	{
	    perror("P__opendev: dup r");
	    kill (0, SIGKILL) ;	/* kill parent too */
	    _exit(127) ;
	}

	if (dup(p2[WRITE]) != 1) 
	{
	    perror("P__opendev: dup w");
	    kill (0, SIGKILL) ;
	    _exit(127) ;
	}

	execvp(pgm, argv);
    /* if we get here, the exec failed */
	fprintf (stderr,"can't execute ");
	perror (pgm);
        kill (0, SIGKILL) ;
	_exit(127);
    }
}

P__readdev (buf, n)
    char *buf;
{
    int i;

    P__flushdev ();
    while (n > 0)
    {
	i = read (rfd, buf, n);
	if (i <= 0)
	    P__errordev ("can't read from");
	buf += i;
	n -= i;
    }
}

P__writedev (buf, n)
    unsigned char *buf;
{
    register int count;
    register int nleft;
    register unsigned char *b;
    register int i ;

    for (nleft = n; nleft > 0; nleft -= count)
    {

/* first flush the buffer if full */
	if (xcount >= sizeof (xbuf))
	    P__flushdev ();

/* copy as much user data as will fit into device buf */
	count = sizeof (xbuf)  - xcount ;
	if(count > nleft)
	    count = nleft;
	b = &xbuf[xcount] ;
	for (i = 0; i < count; i++)
	    *b++ = *buf++;
	xcount += count;
    }
}
