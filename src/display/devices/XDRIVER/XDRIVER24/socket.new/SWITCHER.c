
/* Changed for truecolor 24bit support by 
 * Roberto Flor/ITC-Irst, Trento, Italy
 * August 1999
 *
 * added new parameter "nlev" to specify number of colors per color channel
 * example; nlev=256 means 8bit for each R, G, B equal to 24bit truecolor
*/
     

/* - This driver extensively updated for ICCCM compliance, lint, etc.
 * by P. Thompson (phils@athena.mit.edu) on 9/13/90 - Driver modified
 * to work with Decstation X11r3 server. by David B. Satnik
 * (SATNIKDB%cwu.bitnet) on 8/90 */

#include "gis.h"
#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/time.h>
#include "includes.h"

#include "../lib/graph.h"
#define SWITCHER
#include "../lib/driver.h"
#include "win.h"
#undef SWITCHER

#define BYTE unsigned char
#define REC(a,b)    if ((eof=rec((char*)(a),(int)(b)))) break
#define SEND(a,b)   _send((char*)(a),(int)(b))
#define RECTEXT(x)  if ((eof=rectext(x))) break

#include "pad.h"

static char *me, *sockpath;
static int _wfd, _rfd;
static int eof;
static int n_read;
static int atbuf;
static char inbuf[4096];
static char current_command;
PAD *padlist;            /* user created pads */
PAD *curpad;             /* current selected pad */
static int get_command(char *);
static int rec(char *,int);
static int rectext(char *);
static int _rectext(char *);
static int get1(char *);
static int read1(char *);
static int _send(char *, int);
static int sendtext(char *);
static void close_mon();
static char *xalloc(char *,int *,int,int);
static ITEM *new_item(PAD *,char *);
static ITEM *find_item(PAD *, char *);
static int free_item(ITEM *);
static int invent_pad(char *);
static int delink_pad(PAD *);
static char *store(char *);
static int RESULT(BYTE);

#define LOOP_PER_SERVICE 15

int main (int argc, char *argv[])
{
    char c, lc = 0;
    char name[200], text[1024];
    float wx;
    int *xarray = NULL, *yarray = NULL;
    int button;
    int index, number;
    int min, max;
    pid_t pid;
    fd_set  fd_watch;
    int syntax, listenfd;
    int nlev;
    int sts, waitime, opened;
    int t, b, l, r;
    int x, y;
    static int blu_alloc = 0, grn_alloc = 0, red_alloc = 0;
    static int n_xarray = 0, n_yarray = 0;
    BYTE *blua = NULL, *grna = NULL, *reda = NULL;
    BYTE blu = 0, grn = 0, red = 0;
    ITEM *item;
    LIST *list;
    PAD *pad;
    /* How many commands for each check of Xevents?? */
    static int cmd_loop_count;  
    struct timeval timeout;
    struct sigaction  mysig_close;
    sigset_t  m_close;

    /* whoami */
    me = argv[0];
    /* initialize the pads */
    padlist = NULL;
    curpad = NULL;
    create_pad("");             /* scratch pad */

    /* The calling syntax is as follows: monitor_name [-] "input_fifo
     * output_fifo" The "-", if present, causes the monitor to run in
     * foreground. Otherwise, once it has been determined that the
     * monitor is not already running, we will fork and the parent will
     * exit, so that the monitor is left running in background.  For
     * more information, see the comments in Dstart_mon.c. */

    /* Calling syntax updated: just need monitor_name, we can get the
     * rest. We never can run in the foreground... Optionally have nlevs.
     */

    syntax = 0;
    nlev=32;
    switch (argc) 
    {  /* syntax check */  
	case 1:
	    break;
        case 2:
            nlev = atoi (argv[1]);
	    if (nlev == 0) 
		syntax = 1;
            break;
        default:
            syntax = 1;
    }
    if (syntax) {
        fprintf(stderr, "Usage: %s [nlev]\n", me);
        exit(-1);
    }

    /*******************
     * Get the full path to the unix socket
     *******************/
    
    if (NULL == (sockpath = G_sock_get_fname (me)))
    {
	fprintf (stderr, "Error getting socket path for monitor <%s>\n", me);
	exit (EXIT_FAILURE);
    }

    /*****************
     * Make sure it's not already in use...
     *****************/
    if (G_sock_exists(sockpath))
    {
        if ((listenfd = G_sock_connect(sockpath)) != -1)
        {
	    close (listenfd);
	    fprintf (stderr, "Monitor <%s> is already running", me);
            exit (EXIT_FAILURE);
        }
        unlink (sockpath);
    }
    
    /*****************
     * Try to bind to the unix socket.  This will fail if another process
     * already has it bound.  We'll listen after we make fork(s).
     ****************/
    
    if ( (listenfd = G_sock_bind (sockpath)) == -1)
    {
        fprintf (stderr, "Can't bind to socket for monitor <%s>. "\
			"Already in use?\n", me);
	exit (EXIT_FAILURE);
    }

    /* initialize graphics.  */
    if ( nlev == 0 ) {
	fprintf(stderr,"Nlev is zero, resetting to 32\n");
	nlev=32;
    }
/* pass nlev=-1 to Graph_Set as a flag to try to force TrueColor  */;
    if ( nlev < -1 ) {
	fprintf(stderr,"Nlev is negative, resetting to 32\n");
	nlev=32;
    }
    if ( nlev > 256 ) {
	fprintf(stderr,"Nlev is too big ( > 256), resetting to 32\n");
	nlev=32;
    }
    if (!Graph_Set(argc, argv,nlev))
        exit(-1);

    /* Initialize color map stuff */
    Color_table_fixed();
    waitime = 1;

    /* We are free to run now in the background */
    if ((pid = fork())) 
    {
	if (pid > 0) {      /* parent exits */
	    fprintf(stderr, "Graphics driver [%s] started\n", me);
	    exit(0);
	} else {            /* weren't able to fork */
	    fprintf(stderr, "Error - Can't fork to start [%s]\n", me);
	    exit(-1);
	}
    }
    
    /* change process groups to be shielded from keyborad
     * signals note: use BSD form of call, which will also work
     * for ATT */
    /* setpgrp(0, getpid());*/
    setsid();

    sigemptyset(&m_close);
    mysig_close.sa_handler = close_mon;
    mysig_close.sa_mask = m_close;
    mysig_close.sa_flags = 0;
    sigaction(SIGTERM, &mysig_close, NULL);
    /* signal(SIGTERM, close_mon); */ /* exit gracefully if terminated */


    if (G_sock_listen (listenfd, 1) != 0)
    {
	G_fatal_error ("In %s: G_sock_listen() got error %s\n", 
	        __FILE__, strerror (errno));
    }

    for (;;) {                 /* re-open upon EOF */
    
	for (;;)
	{
	    FD_ZERO (&fd_watch);
	    FD_SET (listenfd, &fd_watch);
	    
	    timeout.tv_sec = 0;
	    timeout.tv_usec = 10000;
	    if ( (opened = select (FD_SETSIZE,  
			    &fd_watch, NULL, NULL, &timeout) ) == 1)
	    {
		_rfd = _wfd = G_sock_accept (listenfd);
		if (_rfd == -1 && errno == EINTR)
		    continue;
		else if (_rfd == -1)
		{
		    Graph_Close();
		    exit (EXIT_FAILURE);
		}
		break; /* out of inner for(;;) */
	    }
	    else if (!opened)
	    {
                Service_Xevent(); 
		XNoOp(dpy);            /* see if X is still running */
	    }
            else  /* opened == -1 */
	    {  /* error on select() call, exit */
		exit (EXIT_FAILURE);
	    }
        }

        atbuf = n_read = 0;

        eof = 0;
        current_command = 0;
        cmd_loop_count  = 1;


        /* loop until getting an eof on the fifo, checking alternately
         * for an X event to handle and for something to read on the
         * fifo. */
        while (eof <= 0) 
        {
            if(--cmd_loop_count==0) 
            {
                Service_Xevent();  /* take care of any events */
                cmd_loop_count = LOOP_PER_SERVICE;
            }

            sts = get_command(&c);
            if (sts == 1) 
            {     /* see if EOF from socket */
                break;
            }
            else if (sts == -1)
            {
                Service_Xevent();  /* take care of any events */
                continue;       /* if timed out repeat the loop */
            }

            /* if we get this far we have received something */
            switch (c) {
            case BEGIN:
                c = 0;
                for (index = -10; index < BEGIN_SYNC_COUNT; index++)
                    SEND(&c, 1);
                c = COMMAND_ESC;
                SEND(&c, 1);
                break;
            case RESPOND:
                XSync(dpy, 1);	
                SEND(&c, 1);
                break;
            case GET_NUM_COLORS:
                Number_of_colors(&index);
                SEND(&index, sizeof index);
                break;
            case STANDARD_COLOR:
               REC(&index, sizeof index);
                Standard_color(index);
                break;
            case COLOR:
                REC(&index, sizeof index);
                Color(index);
                break;
            case RGB_COLOR:
                REC(&red, sizeof red);
                REC(&grn, sizeof grn);
                REC(&blu, sizeof blu);
                RGB_color(red, grn, blu);
                break;
            case COLOR_TABLE_FIXED:
                x = Color_table_fixed();
                SEND(&x, sizeof x);
                break;
            case COLOR_TABLE_FLOAT:
                x = Color_table_float();
                SEND(&x, sizeof x);
                break;
            case COLOR_OFFSET:
                REC(&index, sizeof index);
                Color_offset(index);
                break;
            case COLOR_PRINT:
                break;
            case CONT_ABS:
                REC(&x, sizeof x);
                REC(&y, sizeof y);
                Cont_abs(x, y);
                break;
            case CONT_REL:
                REC(&x, sizeof x);
                REC(&y, sizeof y);
                Cont_rel(x, y);
                break;
            case BOX_ABS:
                REC(&l, sizeof l);
                REC(&t, sizeof t);
                REC(&r, sizeof r);
                REC(&b, sizeof b);
                Box_abs(l, t, r, b);
                break;
            case BOX_REL:
                REC(&l, sizeof l);
                REC(&t, sizeof t);
                Box_rel(l, t);
                break;
            case ERASE:
                Erase();
                break;
            case GET_LOCATION_WITH_BOX:
                REC(&t, sizeof t);
                REC(&b, sizeof b);
                REC(&x, sizeof x);
                REC(&y, sizeof y);
                Get_location_with_box(t, b, &x, &y, &button);
                SEND(&x, sizeof x);
                SEND(&y, sizeof y);
                SEND(&button, sizeof button);
                break;
            case GET_LOCATION_WITH_LINE:
                REC(&t, sizeof t);
                REC(&b, sizeof b);
                REC(&x, sizeof x);
                REC(&y, sizeof y);
                Get_location_with_line(t, b, &x, &y, &button);
                SEND(&x, sizeof x);
                SEND(&y, sizeof y);
                SEND(&button, sizeof button);
                break;
            case GET_LOCATION_WITH_POINTER:
                REC(&x, sizeof x);
                REC(&y, sizeof y);
                REC(&button, sizeof button);
                Get_location_with_pointer(&x, &y, &button);
                SEND(&x, sizeof x);
                SEND(&y, sizeof y);
                SEND(&button, sizeof button);
                break;
            case GRAPH_CLOSE:
                Graph_Close();
                exit(0);
            case LINEMOD:
                REC(&index, sizeof index);
                /* Linemod(index); */
                break;
            case MOVE_ABS:
                REC(&x, sizeof x);
                REC(&y, sizeof y);
                Move_abs(x, y);
                break;
            case MOVE_REL:
                REC(&x, sizeof x);
                REC(&y, sizeof y);
                Move_rel(x, y);
                break;
            case RASTER_CHAR:
                REC(&x, sizeof x);
                REC(&y, sizeof y);
                REC(&index, sizeof index);
                blua = (BYTE *) xalloc((char *) blua, &blu_alloc, x,
                        sizeof *blua);
                REC(blua, x * sizeof(char));
                if (index != 0) index = 1;
                Raster_char(x, y, blua, index, 1);
                break;
            case RASTER_INT:
                REC(&x, sizeof x);
                REC(&y, sizeof y);
                REC(&index, sizeof index);
                xarray = (int *) xalloc((char *) xarray, &n_xarray, x,
                        sizeof(*xarray));
                REC(xarray, x * sizeof(*xarray));
                if (index != 0)
                    index = 1;
                Raster_int(x, y, xarray, index, 1);
                break;
            case RGB_RASTER:
                REC(&x, sizeof x);
                REC(&y, sizeof y);
                reda = (BYTE *) xalloc((char *) reda, &red_alloc, x,
                        sizeof(*reda));
                grna = (BYTE *) xalloc((char *) grna, &grn_alloc, x,
                        sizeof(*grna));
                blua = (BYTE *) xalloc((char *) blua, &blu_alloc, x,
                        sizeof(*blua));
                REC(reda, x * sizeof(char));
                REC(grna, x * sizeof(char));
                REC(blua, x * sizeof(char));
                REC(&t, sizeof t);
                RGB_raster(x, y, reda, grna, blua, t);
                break;
            case RGB_COLORS:
                reda = (BYTE *) xalloc((char *) reda, &red_alloc, 256,
                        sizeof(*reda));
                grna = (BYTE *) xalloc((char *) grna, &grn_alloc, 256,
                        sizeof(*grna));
                blua = (BYTE *) xalloc((char *) blua, &blu_alloc, 256,
                        sizeof(*blua));
                REC(reda, 256);
                REC(grna, 256);
                REC(blua, 256);
                Set_RGB_color(reda, grna, blua);
                break;
            case POLYGON_ABS:
                REC(&number, sizeof number);
                xarray = (int *) xalloc((char *) xarray, &n_xarray, number,
                        sizeof(*xarray));
                yarray = (int *) xalloc((char *) yarray, &n_yarray, number,
                        sizeof(*yarray));
                REC(xarray, number * sizeof(xarray[0]));
                REC(yarray, number * sizeof(yarray[0]));
                Polygon_abs(xarray, yarray, number);
                break;
            case POLYGON_REL:
                REC(&number, sizeof number);
                xarray = (int *) xalloc((char *) xarray, &n_xarray, number,
                        sizeof(*xarray));
                yarray = (int *) xalloc((char *) yarray, &n_yarray, number,
                        sizeof(*yarray));
                REC(xarray, number * sizeof(xarray[0]));
                REC(yarray, number * sizeof(yarray[0]));
                Polygon_rel(xarray, yarray, number);
                break;
            case POLYLINE_ABS:
                REC(&number, sizeof number);
                xarray = (int *) xalloc((char *) xarray, &n_xarray, number,
                        sizeof(*xarray));
                yarray = (int *) xalloc((char *) yarray, &n_yarray, number,
                        sizeof(*yarray));
                REC(xarray, number * sizeof(xarray[0]));
                REC(yarray, number * sizeof(yarray[0]));
                Polyline_abs(xarray, yarray, number);
                break;
            case POLYLINE_REL:
                REC(&number, sizeof number);
                xarray = (int *) xalloc((char *) xarray, &n_xarray, number,
                        sizeof(*xarray));
                yarray = (int *) xalloc((char *) yarray, &n_yarray, number,
                        sizeof(*yarray));
                REC(xarray, number * sizeof(xarray[0]));
                REC(yarray, number * sizeof(yarray[0]));
                Polyline_rel(xarray, yarray, number);
                break;
            case POLYDOTS_ABS:
                REC(&number, sizeof number);
                xarray = (int *) xalloc((char *) xarray, &n_xarray, number,
                        sizeof(*xarray));
                yarray = (int *) xalloc((char *) yarray, &n_yarray, number,
                        sizeof(*yarray));
                REC(xarray, number * sizeof(xarray[0]));
                REC(yarray, number * sizeof(yarray[0]));
                Polydots_abs(xarray, yarray, number);
                break;
            case POLYDOTS_REL:
                REC(&number, sizeof number);
                xarray = (int *) xalloc((char *) xarray, &n_xarray, number,
                        sizeof(*xarray));
                yarray = (int *) xalloc((char *) yarray, &n_yarray, number,
                        sizeof(*yarray));
                REC(xarray, number * sizeof(xarray[0]));
                REC(yarray, number * sizeof(yarray[0]));
                Polydots_rel(xarray, yarray, number);
                break;
            case RESET_COLORS:
                REC(&min, sizeof min);
                REC(&max, sizeof max);
                if (min > max) {
                    sprintf(text,
                            "min > max on call to reset_colors. min:%d max: %d",
                            min, max);
                    perror(text);
                    exit(-1);
                }
                if ((number = max - min + 1) > red_alloc) {
                    reda = (BYTE *)
                            xalloc((char *) reda, &red_alloc, number,
                            sizeof(*reda));
                    grna = (BYTE *)
                            xalloc((char *) grna, &grn_alloc, number,
                            sizeof(*grna));
                    blua = (BYTE *)
                            xalloc((char *) blua, &blu_alloc, number,
                            sizeof(*blua));
                }
                REC(reda, number * sizeof(char));
                REC(grna, number * sizeof(char));
                REC(blua, number * sizeof(char));
                Reset_colors(min, max, reda, grna, blua);
                break;
            case RESET_COLOR:
                REC(&red, sizeof red);
                REC(&grn, sizeof grn);
                REC(&blu, sizeof blu);
                REC(&number, sizeof number);
                Reset_color(red, grn, blu, number);
                break;
            case SCREEN_LEFT:
                Screen_left(&index);
                SEND(&index, sizeof index);
                break;
            case SCREEN_RITE:
                Screen_rite(&index);
                SEND(&index, sizeof index);
                break;
            case SCREEN_BOT:
                Screen_bot(&index);
                SEND(&index, sizeof index);
                break;
            case SCREEN_TOP:
                Screen_top(&index);
                SEND(&index, sizeof index);
                break;
            case SET_WINDOW:
                REC(&t, sizeof t);
                REC(&b, sizeof b);
                REC(&l, sizeof l);
                REC(&r, sizeof r);
                Set_window(t, b, l, r);
                break;
            case GET_TEXT_BOX:
                RECTEXT(text);
                Get_text_box(text, &t, &b, &l, &r);
                SEND(&t, sizeof t);
                SEND(&b, sizeof b);
                SEND(&l, sizeof l);
                SEND(&r, sizeof r);
                break;
            case FONT:
                RECTEXT(text);
                x = Font_get(text);
                SEND(&x, sizeof(x));
                break;
            case TEXT:
                RECTEXT(text);
                Text(text);
                break;
            case TEXT_SIZE:
                REC(&x, sizeof x);
                REC(&y, sizeof y);
                Text_size(x, y);
                break;
            case TEXT_ROTATION:
                REC(&wx, sizeof wx);
                Text_rotation(wx);
                break;
            case PANEL_SAVE:
                RECTEXT(text);
                REC(&t, sizeof t);
                REC(&b, sizeof b);
                REC(&l, sizeof l);
                REC(&r, sizeof r);
                Panel_save(text, t, b, l, r);
                break;
            case PANEL_RESTORE:
                RECTEXT(text);
                Panel_restore(text);
                break;
            case PANEL_DELETE:
                RECTEXT(text);
                Panel_delete(text);
                break;
            case PAD_CREATE:
                RECTEXT(text);
                if (*text == 0) /* this is scratch pad */
                    RESULT(OK);
                else if (find_pad(text) != NULL)
                    RESULT(DUPLICATE);  /* duplicate pad */
                else if (create_pad(text))
                    RESULT(OK);
                else
                    RESULT(NO_MEMORY);
                break;

            case PAD_CURRENT:
                if (curpad == NULL) {
                    RESULT(NO_CUR_PAD);
                    sendtext("");
                } else {
                    RESULT(OK);
                    sendtext(curpad->name);
                }
                break;

            case PAD_DELETE:
                if (curpad == NULL)
                    RESULT(NO_CUR_PAD);
                else if (*curpad->name == 0)
                    RESULT(ILLEGAL);
                else {
                    delete_pad(curpad);
                    curpad = NULL;
                    RESULT(OK);
                }
                break;

            case PAD_INVENT:
                invent_pad(text);
                sendtext(text);
                break;

            case PAD_LIST:
                for (pad = padlist; pad != NULL; pad = pad->next)
                    if (*pad->name)
                        sendtext(pad->name);
                sendtext("");
                break;

            case PAD_SELECT:
                RECTEXT(text);  /* pad name */
                curpad = find_pad(text);
                if (curpad == NULL)
                    RESULT(NO_PAD);
                else 
                    RESULT(OK);
                break;

            case PAD_GET_ITEM:
                RECTEXT(text);  /* item name */
                if (curpad == NULL) {
                    RESULT(NO_CUR_PAD);
                    break;
                }
                item = find_item(curpad, text);
                if (item == NULL) {
                    RESULT(NO_ITEM);
                    break;
                }
                RESULT(OK);
                for (list = item->list; list != NULL; list = list->next)
                    if (*list->value) 
                        sendtext(list->value);
                sendtext("");
                break;

            case PAD_SET_ITEM:
                RECTEXT(name);  /* item name */
                RECTEXT(text);  /* item value */
                if (curpad == NULL) {
                    RESULT(NO_CUR_PAD);
                    break;
                }
                delete_item(curpad, name);
                if (append_item(curpad, name, text))
                    RESULT(OK);
                else
                    RESULT(NO_MEMORY);
                break;

            case PAD_APPEND_ITEM:
                RECTEXT(name);  /* item name */
                RECTEXT(text);  /* item value */
                if (curpad == NULL) {
                    RESULT(NO_CUR_PAD);
                    break;
                }
                if (append_item(curpad, name, text))
                    RESULT(OK);
                else
                    RESULT(NO_MEMORY);
                break;

            case PAD_DELETE_ITEM:
                RECTEXT(text);  /* item name */
                if (curpad == NULL) {
                    RESULT(NO_CUR_PAD);
                    break;
                }
                delete_item(curpad, text);
                RESULT(OK);
                break;

            case PAD_LIST_ITEMS:
                if (curpad == NULL) {
                    RESULT(NO_CUR_PAD);
                    break;
                }
                RESULT(OK);
                for (item = curpad->items; item != NULL; item = item->next)
                    if (*item->name)
                        sendtext(item->name);
                sendtext("");
                break;

            default:
                fprintf(stderr, "\nUnknown command: %d last: %d", c, lc);
                break;
            }
            lc = c;
        }                       /* end of the "while (eof<=0) */
        /* read encountered EOF. close socket now */
        close(_wfd); 
        close(_rfd);
    } /* end of the "for(;;)" way back */
}

static int get_command(char *c)
{
    /* is there a command char pending? */
    if ((*c = current_command)) {
        current_command = 0;
        return 0;
    }
    /* read the socket. look for 1 (or more) COMMAND_ESC chars followed
     * by a non-zero command token char. If there is an eof return 1,
     * else return 0. */
    while (read1(c) == 0) {     /* while not EOF */
        if (*c != COMMAND_ESC)
            continue;
        while (*c == COMMAND_ESC)
            if (read1(c) != 0)
                return 1;
        if (*c)
            return 0;           /* got the command token */
    }
    return 1;                   /* EOF */
}


static int rec(char *buf, int n)
{
    int stat;

    while (n-- > 0) {
        if ((stat = get1(buf++)) != 0)
            return stat;        /* EOF or COMMAND_ESC */
    }
    return 0;
}

static int rectext(char *s)
{
    int stat;
    char buf[80];

    stat = _rectext(s);
    sprintf(buf, "  stat=%d\n", stat);
    return stat;
}

static int _rectext(char *s)
{
    /* Read byte-by-byte into the buffer until '\0' */
    int stat;

    while ((stat = get1(s)) == 0)
        if (*s++ == '\0')
            return 0;
    return stat;                /* EOF or COMMAND_ESC */
}

static int get1(char *c)
{
    if (read1(c) != 0) {
        return 1;               /* EOF */
    }
    if (*c != COMMAND_ESC) {
        return 0;               /* OK */
    }
    if (read1(c) != 0) {
        return 1;               /* EOF */
    }
    if (*c) {
        current_command = *c;
        return -1;              /* Got command within data */
    }
    *c = COMMAND_ESC;           /* sequence COMMAND_ESC,0 becomes data
                                 * COMMAND_ESC */
    return 0;                   /* OK */
}

static int read1(char *c)
{
    if (atbuf == n_read) {
        atbuf = 0;
        n_read = read(_rfd, inbuf, sizeof inbuf);
        if (n_read <= 0)
            return 1;           /* EOF */
    }
    *c = inbuf[atbuf++];
    return 0;
}


static int _send(char *buf, int n)
{
    (void) write(_wfd, buf, n);
    return 0;
}

static int sendtext(char *s)
{
    SEND(s, strlen(s) + 1);
    return 0;
}

static char *store(char *s)
{
    char *buf;

    buf = (char *)G_malloc((size_t) (strlen(s) + 1));
    if (buf != NULL)
        strcpy(buf, s);
    return buf;
}

static int RESULT(BYTE n)
{
    BYTE c;

    c = n;
    SEND(&c, 1);
    return 0;
}

/*************** pad routines ************************************/
int create_pad (char *name)
{
    PAD *pad;

    pad = (PAD *) G_malloc((size_t) sizeof(PAD));
    if (pad == NULL)
        return 0;
    pad->name = store(name);
    if (pad->name == NULL) {
        free((char *) pad);
        return 0;
    }
    pad->items = NULL;
    pad->next = padlist;
    if (pad->next != NULL)
        pad->next->prev = pad;
    pad->prev = NULL;
    padlist = pad;
    return 1;
}

static int delink_pad(PAD *pad)
{
    if (pad == NULL)
        return 1;

    if (pad->prev == NULL)
        padlist = pad->next;
    else
        pad->prev->next = pad->next;

    if (pad->next != NULL)
        pad->next->prev = pad->prev;

    return 0;
}

int 
delete_pad (PAD *pad)
{
    ITEM *item, *next;

    if (pad == NULL)
        return 0;

    delink_pad(pad);

    /* free the items */
    for (item = pad->items; item != NULL; item = next) {
        next = item->next;
        free_item(item);
    }
    free((char *) pad);

    return 1;
}

PAD *
find_pad (char *name)
{
    PAD *pad;

    for (pad = padlist; pad != NULL; pad = pad->next)
        if (strcmp(name, pad->name) == 0)
            return pad;
    return (PAD *) NULL;
}

static int invent_pad(char *name)
{
    static int i = 0;

    do
        sprintf(name, "%d", ++i);
    while (find_pad(name) != NULL);

    return 0;
}

int append_item (PAD *pad, char *name, char *value)
{
    ITEM *item;
    LIST *cur, *prev;
    LIST *list;

    if (pad == NULL)
        return 0;

    /* allocate a list struct and put value into it */
    list = (LIST *) G_malloc((size_t) sizeof(LIST));
    if (list == NULL)
        return 0;
    list->next = NULL;
    list->value = store(value);
    if (list->value == NULL) {
        free((char *) list);
        return 0;
    }
    /* find the named item for the current pad */
    item = find_item(pad, name);
    if (item == NULL)
        item = new_item(pad,name);
    if (item == NULL)
        return 0;

    /* add the LIST at the end of the item LIST */
    prev = NULL;
    for (cur = item->list; cur != NULL; cur = cur->next)
        prev = cur;

    if (prev == NULL)
        item->list = list;
    else
        prev->next = list;

    return 1;
}

int delete_item (PAD *pad, char *name)
{
    ITEM *item;

    item = find_item(pad, name);
    if (item == NULL)
        return 0;

    if (item->prev == NULL)
        pad->items = item->next;
    else
        item->prev->next = item->next;

    if (item->next != NULL)
        item->next->prev = item->prev;

    /* free the item */
    free_item(item);

    return 1;
}

static int free_item(ITEM *item)
{
    LIST *list, *next;

    if (item->name != NULL)
        free((char *) item->name);
    for (list = item->list; list != NULL; list = next) {
        next = list->next;
        if (list->value)
            free((char *) list->value);
        free((char *) list);
    }
    free((char *) item);

    return 0;
}


static ITEM *find_item(PAD *pad, char *name)
{
    ITEM *item;

    if (pad != NULL)
        for (item = pad->items; item != NULL; item = item->next)
            if (strcmp(name, item->name) == 0)
                return item;
    return (ITEM *) NULL;
}


static ITEM *new_item(PAD *pad, char *name)
{
    ITEM *item;

    item = (ITEM *) G_malloc((size_t) sizeof(ITEM));
    if (item == NULL)
        return (ITEM *) NULL;

    item->name = store(name);
    if (item->name == NULL) {
        free((char *) item);
        return (ITEM *) NULL;
    }
    item->list = NULL;
    item->next = pad->items;
    if (item->next != NULL)
        item->next->prev = item;
    item->prev = NULL;
    pad->items = item;

    return item;
}

static void 
close_mon (void)
{
    Graph_Close();
}

static char *xalloc(char *buf,int *cur,int new,int len)
{
    if (*cur >= new)
        return buf;
    if (*cur)
        buf = (char *)G_realloc((void *) buf, (size_t) (new * len));
    else
        buf = (char *)G_malloc((size_t) (new * len));
    *cur = new;
    if (buf == NULL) {
        fprintf(stderr, "%s: Out of Memory\n", me);
        exit(1);
    }
    return buf;
}

/* vim: set softtabstop=4 shiftwidth=4 expandtab: */
/*** end SWITCHER.c ***/
