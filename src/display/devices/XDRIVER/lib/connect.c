#include <stdio.h>
#include <setjmp.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/stat.h>
#ifdef __CYGWIN__
#define MODE 0644
#else
#define MODE 0666
#endif

int get_connection(files, rfd, wfd)
char *files;
int *rfd, *wfd;
{
    int keyin, keyout, midin, midout;
    char input[1024], output[1024];

    sscanf(files, "%s %s", input, output);
    keyin = ftok(input,0);
    keyout = ftok(output, 0);
    midin = msgget(keyin , IPC_CREAT | 0600);
    *rfd = midin;
    if (midin == -1) {
        perror(input);
        exit(-1);
    }
    midout = msgget(keyout, IPC_CREAT | 0600);
    *wfd = midout;
    if (midout == -1) {
        perror(output);
        exit(-1);
    }
    
    return 0;
}

int prepare_connection(void)
{
    return 0;
}

/* check_connection - see if we are already running */
/* this goes in the same place as prepare_connection and
 * get_connection. */
/* it is called by SWITCHER first to check whether or not the driver */
/* is already running.  link contains the names of the input and output */
/* fifos (for AT&T, socket and mode for Berkley) separated by a space. */
/* return is -1 if something is using the driver's communication */
/* channel, 0 otherwise. */
/* this is the AT&T version. */

static jmp_buf save;
static void timeout(int);

int check_connection(me, link)
char *me, *link;
{
    struct stat buf;
    char in_fifo[1024], out_fifo[1024];
    int time, in_file, out_file;
    void (*def) () = NULL;

    time = 1;                  /* time to wait for opens below */
    sscanf(link, "%s %s", in_fifo, out_fifo);
    /* Check existence and access of in_fifo */
    if (-1 == stat(in_fifo, &buf)) {
        fprintf(stderr, "Sorry, <%s> not available\n", in_fifo);
        goto error;
    }
    if ((buf.st_mode & MODE) != MODE) {
        fprintf(stderr, "Sorry, permissions on <%s> (%o) should be %o\n",
                in_fifo, buf.st_mode & MODE, MODE);
        goto error;
    }
    if (-1 == stat(out_fifo, &buf)) {
        fprintf(stderr, "Sorry, <%s> not available\n", out_fifo);
        goto error;
    }
    if ((buf.st_mode & MODE) != MODE) {
        fprintf(stderr, "Sorry, permissions on <%s> (%o) should be %o\n",
                out_fifo, buf.st_mode & MODE, MODE);
        goto error;
    }
    out_file = msgget(ftok(in_fifo,0), 0600); /* reading here? */
    in_file = msgget(ftok(out_fifo,0), 0600); /* writing here? */
    if( (out_file == -1) || (in_file == -1) )
	return (0);

    fprintf(stderr, "Graphics driver [%s] is already running\n", me);
    fflush(stderr);
    return (-1);                /* tell caller someone's listening */
  error:
    fprintf(stderr, "Have GRASS adminstrator check etc/monitorcap file\n");
    fflush(stderr);
    return (-2);

}

static void timeout(int dummy)
{
    longjmp(save, -1);
}

