
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>
#include <unistd.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <time.h>

#include "gis.h"
#include "graph.h"
#include "driverlib.h"
#include "driver.h"
#include "pad.h"
#include "utils.h"

#define REC(a,b)    if ((eof=rec((a),(b)))) break
#define RECTEXT(x,s)  if ((eof=rectext(&x,&s))) break
#define SEND(a,b)   _send((a),(b))
#define SENDTEXT(x)  sendtext((x))

static int rec(void *,int);
static int rectext(char **,int *);
static int _send(void *,int);
static int sendtext(char *);
static int RESULT(int);

static int _wfd;
static int _rfd;

static char inbuf[4096];
static int n_read;
static int atbuf;

static char current_command;

static int eof;

static PAD *curpad;       /* current selected pad */

static unsigned char *blua;
static unsigned char *grna;
static unsigned char *reda;
static unsigned char *nula;
static int blu_alloc;
static int grn_alloc;
static int red_alloc;
static int nul_alloc;

static int *xarray;
static int *yarray;
static int n_xarray;
static int n_yarray;

static char lc;

void
command_init(int rfd, int wfd)
{
    _rfd = rfd;
    _wfd = wfd;

    atbuf = n_read = 0;
    current_command = 0;

    eof = 0;
}

int
command_get_input(void)
{
    return _rfd;
}

int
process_command(int c)
{
    static char *name;
    static int name_size;
    static char *text;
    static int text_size;

    int t, b, l, r, ret;
    int x, y;
    unsigned char red, grn, blu;
    int min, max;
    int number;
    int index;
    int button;
    float wx;
    ITEM *item;
    LIST *list;
    PAD *pad;
    unsigned char ch;
    struct timespec delay,remains;
    

    /* we sleep a bit when querying maps or zooming into - to reduce CPU load */
    delay.tv_sec=0;
    delay.tv_nsec=500000; /* 500 milliseconds */

    switch(c)
    {
    case BEGIN:
	ch = 0;
	for (index = -10; index < BEGIN_SYNC_COUNT; index++)
	    SEND(&ch, 1);
	ch = COMMAND_ESC;
	SEND(&ch, 1);
	break;
    case RESPOND:
	Respond();
	SEND(&ch, 1);
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
	Get_location_with_box2(t, b, &x, &y, &button, 1); /* start */
        while ( 1 ) {
	    /* Check monitor and send result back */
	    ret = Get_location_with_box2(t, b, &x, &y, &button, 2);
	    if ( ret == 0 ) button = 0;
	    /* we send back current position but button set to 0 */
	    SEND(&x, sizeof x);
	    SEND(&y, sizeof y);
	    SEND(&button, sizeof button);
	    if ( ret == 1 ) break;
	    /* Should we continue (0) or break (1) */
	    REC(&ret, sizeof ret );
	    if ( ret == 1 ) { /* break */
	        ret = Get_location_with_box2(t, b, &x, &y, &button, 3);
            	break;
	    }
	    nanosleep(&delay,&remains); /* sleep to slow down loop */
	}	
	break;
    case GET_LOCATION_WITH_LINE:
	REC(&t, sizeof t);
	REC(&b, sizeof b);
	REC(&x, sizeof x);
	REC(&y, sizeof y);
	Get_location_with_line2(t, b, &x, &y, &button, 1); /* start */
        while ( 1 ) {
	    /* Check monitor and send result back */
	    ret = Get_location_with_line2(t, b, &x, &y, &button, 2);
	    if ( ret == 0 ) button = 0;
	    /* we send back current position but button set to 0 */
	    SEND(&x, sizeof x);
	    SEND(&y, sizeof y);
	    SEND(&button, sizeof button);
	    if ( ret == 1 ) break;
	    /* Should we continue (0) or break (1) */
	    REC(&ret, sizeof ret );
	    if ( ret == 1 ) { /* break */
	        ret = Get_location_with_line2(t, b, &x, &y, &button, 3);
            	break;
	    }
	    nanosleep(&delay,&remains); /* sleep to slow down loop */
	}	
	break;
    case GET_LOCATION_WITH_POINTER:
	REC(&x, sizeof x);
	REC(&y, sizeof y);
	REC(&button, sizeof button);
	Get_location_with_pointer2(&x, &y, &button, 1); /* start */
        while ( 1 ) {
	    /* Check monitor and send result back */
	    ret = Get_location_with_pointer2(&x, &y, &button, 2);
	    if ( ret == 0 ) button = 0;
	    /* we send back current position but button set to 0 */
	    SEND(&x, sizeof x);
	    SEND(&y, sizeof y);
	    SEND(&button, sizeof button);
	    if ( ret == 1 ) break;
	    /* Should we continue (0) or break (1) */
	    REC(&ret, sizeof ret );
	    if ( ret == 1 ) { /* break */
	        ret = Get_location_with_pointer2(&x, &y, &button, 3);
            	break;
	    }
	    nanosleep(&delay,&remains); /* sleep to slow down loop */
	}	
	break;
    case GRAPH_CLOSE:
	Graph_Close();
	exit(0);
    case LINEMOD:
	REC(&index, sizeof index);
	Linemod(&index);
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
	blua = (unsigned char *) xalloc(blua, &blu_alloc, x, sizeof(*blua));
	REC(blua, x * sizeof(char));
	if (index != 0) index = 1;
	Raster_char(x, y, blua, index, 1);
	break;
    case RASTER_INT:
	REC(&x, sizeof x);
	REC(&y, sizeof y);
	REC(&index, sizeof index);
	xarray = (int *) xalloc(xarray, &n_xarray, x, sizeof(*xarray));
	REC(xarray, x * sizeof(*xarray));
	if (index != 0) index = 1;
	Raster_int(x, y, xarray, index, 1);
	break;
    case RGB_RASTER:
	REC(&x, sizeof x);
	REC(&y, sizeof y);
	reda = (unsigned char *) xalloc(reda, &red_alloc, x, sizeof(*reda));
	grna = (unsigned char *) xalloc(grna, &grn_alloc, x, sizeof(*grna));
	blua = (unsigned char *) xalloc(blua, &blu_alloc, x, sizeof(*blua));
	nula = (unsigned char *) xalloc(nula, &nul_alloc, x, sizeof(*nula));
	REC(reda, x * sizeof(char));
	REC(grna, x * sizeof(char));
	REC(blua, x * sizeof(char));
	REC(nula, x * sizeof(char));
	REC(&t, sizeof t);
	RGB_raster(x, y, reda, grna, blua, t ? nula : NULL);
	break;
    case RGB_COLORS:
	reda = (unsigned char *) xalloc(reda, &red_alloc, 256, sizeof(*reda));
	grna = (unsigned char *) xalloc(grna, &grn_alloc, 256, sizeof(*grna));
	blua = (unsigned char *) xalloc(blua, &blu_alloc, 256, sizeof(*blua));
	REC(reda, 256);
	REC(grna, 256);
	REC(blua, 256);
	Set_RGB_color(reda, grna, blua);
	break;
    case POLYGON_ABS:
	REC(&number, sizeof number);
	xarray = (int *) xalloc(xarray, &n_xarray, number, sizeof(*xarray));
	yarray = (int *) xalloc(yarray, &n_yarray, number, sizeof(*yarray));
	REC(xarray, number * sizeof(xarray[0]));
	REC(yarray, number * sizeof(yarray[0]));
	Polygon_abs(xarray, yarray, number);
	break;
    case POLYGON_REL:
	REC(&number, sizeof number);
	xarray = (int *) xalloc(xarray, &n_xarray, number, sizeof(*xarray));
	yarray = (int *) xalloc(yarray, &n_yarray, number, sizeof(*yarray));
	REC(xarray, number * sizeof(xarray[0]));
	REC(yarray, number * sizeof(yarray[0]));
	Polygon_rel(xarray, yarray, number);
	break;
    case POLYLINE_ABS:
	REC(&number, sizeof number);
	xarray = (int *) xalloc(xarray, &n_xarray, number, sizeof(*xarray));
	yarray = (int *) xalloc(yarray, &n_yarray, number, sizeof(*yarray));
	REC(xarray, number * sizeof(xarray[0]));
	REC(yarray, number * sizeof(yarray[0]));
	Polyline_abs(xarray, yarray, number);
	break;
    case POLYLINE_REL:
	REC(&number, sizeof number);
	xarray = (int *) xalloc(xarray, &n_xarray, number, sizeof(*xarray));
	yarray = (int *) xalloc(yarray, &n_yarray, number, sizeof(*yarray));
	REC(xarray, number * sizeof(xarray[0]));
	REC(yarray, number * sizeof(yarray[0]));
	Polyline_rel(xarray, yarray, number);
	break;
    case POLYDOTS_ABS:
	REC(&number, sizeof number);
	xarray = (int *) xalloc(xarray, &n_xarray, number, sizeof(*xarray));
	yarray = (int *) xalloc(yarray, &n_yarray, number, sizeof(*yarray));
	REC(xarray, number * sizeof(xarray[0]));
	REC(yarray, number * sizeof(yarray[0]));
	Polydots_abs(xarray, yarray, number);
	break;
    case POLYDOTS_REL:
	REC(&number, sizeof number);
	xarray = (int *) xalloc(xarray, &n_xarray, number, sizeof(*xarray));
	yarray = (int *) xalloc(yarray, &n_yarray, number, sizeof(*yarray));
	REC(xarray, number * sizeof(xarray[0]));
	REC(yarray, number * sizeof(yarray[0]));
	Polydots_rel(xarray, yarray, number);
	break;
    case RESET_COLORS:
	REC(&min, sizeof min);
	REC(&max, sizeof max);
	number = max - min + 1;
	reda = (unsigned char *) xalloc(reda, &red_alloc, number, sizeof(*reda));
	grna = (unsigned char *) xalloc(grna, &grn_alloc, number, sizeof(*grna));
	blua = (unsigned char *) xalloc(blua, &blu_alloc, number, sizeof(*blua));
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
	RECTEXT(text, text_size);
	Get_text_box(text, &t, &b, &l, &r);
	SEND(&t, sizeof t);
	SEND(&b, sizeof b);
	SEND(&l, sizeof l);
	SEND(&r, sizeof r);
	break;
    case FONT:
	RECTEXT(text, text_size);
	x = Font_get(text);
	SEND(&x, sizeof x);
	break;
    case TEXT:
	RECTEXT(text, text_size);
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
	RECTEXT(text, text_size);
	REC(&t, sizeof t);
	REC(&b, sizeof b);
	REC(&l, sizeof l);
	REC(&r, sizeof r);
	Panel_save(text, t, b, l, r);
	break;
    case PANEL_RESTORE:
	RECTEXT(text, text_size);
	Panel_restore(text);
	break;
    case PANEL_DELETE:
	RECTEXT(text, text_size);
	Panel_delete(text);
	break;
    case PAD_CREATE:
	RECTEXT(text, text_size);
	if (*text == 0) /* this is scratch pad */
	    RESULT(OK);
	else if (find_pad(text) != NULL)
	    RESULT(DUPLICATE); /* duplicate pad */
	else if (create_pad(text))
	    RESULT(OK);
	else
	    RESULT(NO_MEMORY);
	break;

    case PAD_CURRENT:
	if (curpad == NULL)
	{
	    RESULT(NO_CUR_PAD);
	    SENDTEXT("");
	}
	else
	{
	    RESULT(OK);
	    SENDTEXT(curpad->name);
	}
	break;

    case PAD_DELETE:
	if (curpad == NULL)
	    RESULT(NO_CUR_PAD);
	else if (*curpad->name == 0)
	    RESULT(ILLEGAL);
	else
	{
	    delete_pad(curpad);
	    curpad = NULL;
	    RESULT(OK);
	}
	break;

    case PAD_INVENT:
	invent_pad(text);
	SENDTEXT(text);
	break;

    case PAD_LIST:
	for (pad = pad_list(); pad != NULL; pad = pad->next)
	    if (*pad->name)
		SENDTEXT(pad->name);
	SENDTEXT("");
	break;

    case PAD_SELECT:
	RECTEXT(text, text_size); /* pad name */
	curpad = find_pad(text);
	if (curpad == NULL)
	    RESULT(NO_PAD);
	else
	    RESULT(OK);
	break;

    case PAD_GET_ITEM:
	RECTEXT(text, text_size); /* item name */
	if (curpad == NULL)
	{
	    RESULT(NO_CUR_PAD);
	    break;
	}
	item = find_item(curpad, text);
	if (item == NULL)
	{
	    RESULT(NO_ITEM);
	    break;
	}
	RESULT(OK);
	for (list = item->list; list != NULL; list = list->next)
	    if (*list->value)
		SENDTEXT(list->value);
	SENDTEXT("");
	break;

    case PAD_SET_ITEM:
	RECTEXT(name, name_size); /* item name */
	RECTEXT(text, text_size); /* item value */
	if (curpad == NULL)
	{
	    RESULT(NO_CUR_PAD);
	    break;
	}
	delete_item(curpad, name);
	if (append_item(curpad, name, text, 0))
	    RESULT(OK);
	else
	    RESULT(NO_MEMORY);
	break;

    case PAD_APPEND_ITEM:
	RECTEXT(name, name_size); /* item name */
	RECTEXT(text, text_size); /* item value */
	REC(&index, sizeof index); /* replace flag */
	if (curpad == NULL)
	{
	    RESULT(NO_CUR_PAD);
	    break;
	}
	if (append_item(curpad, name, text, index))
	    RESULT(OK);
	else
	    RESULT(NO_MEMORY);
	break;

    case PAD_DELETE_ITEM:
	RECTEXT(text, text_size); /* item name */
	if (curpad == NULL)
	{
	    RESULT(NO_CUR_PAD);
	    break;
	}
	delete_item(curpad, text);
	RESULT(OK);
	break;

    case PAD_LIST_ITEMS:
	if (curpad == NULL)
	{
	    RESULT(NO_CUR_PAD);
	    break;
	}
	RESULT(OK);
	for (item = curpad->items; item != NULL; item = item->next)
	    if (*item->name)
		SENDTEXT(item->name);
	SENDTEXT("");
	break;

    default:
	fprintf(stderr, "\nUnknown command: %d last: %d\n", c, lc);
	break;
    }
    lc = c;

    return eof;
}

static int read1(char *c)
{
    if (atbuf == n_read)
    {
        atbuf = 0;
        n_read = read(_rfd, inbuf, sizeof inbuf);
	if (n_read < 0)
	    perror("Monitor: read1: Error reading input");
        if (n_read <= 0)
            return 1;           /* EOF */
    }
    *c = inbuf[atbuf++];
    return 0;
}

int get_command(char *c)
{
    /* is there a command char pending? */
    if ((*c = current_command)) {
        current_command = 0;
        return 0;
    }

    /*
     * look for 1 (or more) COMMAND_ESC chars
     * followed by a non-zero comamnd token char
     */
    while (read1(c) == 0)   /* while !EOF */
    {
        if (*c != COMMAND_ESC)
            continue;
        while (*c == COMMAND_ESC)
            if (read1(c) != 0)
	    {
		fprintf(stderr, "Monitor: get_command: Premature EOF\n");
                return 1;               /* EOF */
	    }
        if (*c)
            return 0;           /* got the command token */
    }
    return 1;   /* EOF */
}

static int get1(char *c)
{
    if (read1(c) != 0)
        return 1;       /* EOF */
    if (*c != COMMAND_ESC)
        return 0;       /* OK */
    if (read1(c) != 0)
        return 1;       /* EOF */
    if (*c)
    {
        current_command = *c;
        return -1;      /* Got command within data */
    }
    *c = COMMAND_ESC;   /* sequence COMMAND_ESC,0 becomes data COMMAND_ESC */
    return 0;           /* OK */
}

static int rec(void *buf, int n)
{
    char *cbuf = buf;
    int stat;
    while (n-- > 0) {
        if ((stat=get1(cbuf++)) != 0)
	    return stat; /* EOF or COMMAND_ESC */
    }
    return 0;
}

static int rectext(char **buff_p, int *size_p)
{
    char *buff = *buff_p;
    int size = *size_p;
    int i, stat;
    for (i = 0; ; i++)
    {
	char c;

	stat = get1(&c);
	if (stat != 0)
	    return stat; /* EOF or COMMAND_ESC */

	if (i >= size)
	{
	    *size_p = size = size ? size * 2 : 1000;
	    *buff_p = buff = G_realloc(buff, size);
	}

	buff[i] = c;

	if (!c)
	    return 0;
    }
}

static int _send(void *buf, int n)
{
    int r = write(_wfd, (char *)buf, n);
    if (r < 0)
    {
	perror("Monitor: _send: write");
	return 1;
    }
    if (r < n)
    {
	fprintf(stderr,
		"Monitor: _send: write returned short count: %d of %d\n",
		r, n);
	return 1;
    }
    return 0;
}

static int sendtext(char *s)
{
    SEND(s, strlen(s) + 1);
    return 0;
}

static int RESULT(int n)
{
    unsigned char c;

    c = n;
    SEND(&c, 1);

    return 0;
}

