/*
 *	This software is in the public domain, it may not be resold
 *	or relicensed.  Modified and enhanced versions of this software
 *	are likewise to be made freely available.  Sites using this
 *	software are requested to register with NASA at the address below.  
 *	Send modifications and requests for most recent version to:
 *
 *	Author:  David A. Tristram,  ATTN: Panel Library
 *		 M/S T045-1
 *		 Ames Research Center
 *		 National Aeronautics and Space Administration
 *		 Moffett Field, CA  94035-4000
 *
 *		 415-694-4404
 *		 dat@nas.nasa.gov
 */
/*
 * g_gets.c	: How to read a string from the keyboard without a textport.
 *
 * char *g_gets(stringcolor, backcolor, cursorcolor)
 *		    			         returns stuff like gets() does
 * Colorindex stringcolor, backcolor, cursorcolor;
 *	
 */
#include <string.h>
#include <stdio.h>
#include <gl.h>
#include <device.h>
#include <panel.h>

#define MAXSTRINGLEN 	80
#define MAXQUEUED 	100
#define MAXEVENTS	100
#define MAXDEVICES 	1024
#define RESET		-3
#define ERASE   	-2
#define QUIT		-1

static int termenter = 0;
static int numqueued = 0;
static int numevents = 0;
static int qstack[MAXQUEUED];
static struct {
    Device dev;
    short data;
} eventq[MAXEVENTS];
static long buf_state;

typedef struct {
    int device_num;
    char lower;
    char upper;
} KEY ;

static KEY keyboard[] = 
{
    {	 AKEY,		'a',	'A' },
    {	 BKEY,		'b',    'B' },
    {	 CKEY,		'c',    'C' },
    {	 DKEY,		'd',    'D' },
    {	 EKEY,		'e',    'E' },
    {	 FKEY,		'f',    'F' },
    {	 GKEY,		'g',    'G' },
    {	 HKEY,		'h',    'H' },
    {	 IKEY,		'i',    'I' },
    {	 JKEY,		'j',    'J' },
    {	 KKEY,		'k',    'K' },
    {	 LKEY,		'l',    'L' },
    {	 MKEY,		'm',    'M' },
    {	 NKEY,		'n',    'N' },
    {	 OKEY,		'o',    'O' },
    {	 PKEY,		'p',    'P' },
    {	 QKEY,		'q',    'Q' },
    {	 RKEY,		'r',    'R' },
    {	 SKEY,		's',    'S' },
    {	 TKEY,		't',    'T' },
    {	 UKEY,		'u',    'U' },
    {	 VKEY,		'v',    'V' },
    {	 WKEY,		'w',    'W' },
    {	 XKEY,		'x',    'X' },
    {	 YKEY,		'y',    'Y' },
    {	 ZKEY,		'z',    'Z' },
    {	 ZEROKEY,	'0',    ')' },
    {	 ONEKEY,	'1',    '!' },
    {	 TWOKEY,	'2',    '@' },
    {	 THREEKEY,	'3',    '#' }, 
    {	 FOURKEY,	'4',    '$' }, 
    {	 FIVEKEY,	'5',    '%' }, 
    {	 SIXKEY,	'6',    '^' }, 
    {	 SEVENKEY,	'7',    '&' }, 
    {	 EIGHTKEY,	'8',    '*' }, 
    {	 NINEKEY,	'9',    '(' },
    { 	 SEMICOLONKEY,	';',    ':' },
    {	 PERIODKEY,	'.',    '>' },
    {	 COMMAKEY,	',',    '<' },
    {	 ACCENTGRAVEKEY,'`',    '~' },
    {	 MINUSKEY,	'-',    '_' },
    {	 QUOTEKEY,	'\'',    '"' },
    {	 BACKSLASHKEY,	'\\',    '|' },
    {	 EQUALKEY,	'=',    '+' },
    {	 LEFTBRACKETKEY,'[',    '{' },
    {	 RIGHTBRACKETKEY,']',   '}' },
    {	 VIRGULEKEY,	'/',	'?' },
    {	 SPACEKEY,	' ',    '\0' },
    {	 TABKEY,	'\t',    '\0' },
    {	 RETKEY,	'\r',    '\0' },
    {	 LINEFEEDKEY,	'\n',    '\0' },
    {	 BREAKKEY,	'\0',    '\0' },
    {	 BACKSPACEKEY,	'\b',    '\0' },
    {	 DELKEY,	'\0',    '\0' },
    {	 SETUPKEY,	'\0',    '\0' },
    {	 CTRLKEY,	'\0',    '\0' },
    {	 CAPSLOCKKEY,	'\0',    '\0' },
    {	 RIGHTSHIFTKEY,	'\0',    '\0' },
    {	 LEFTSHIFTKEY,	'\0',    '\0' },
    {	 NOSCRLKEY,	'\0',    '\0' },
    {	 ESCKEY,	'\0',    '\0' },
    {	 LEFTARROWKEY,	'\0',    '\0' },
    {	 DOWNARROWKEY,	'\0',    '\0' },
    {	 RIGHTARROWKEY,	'\0',    '\0' },
    {	 UPARROWKEY,	'\0',    '\0' },
    {	 -1,		'\0',    '\0' } 
};


Screencoord ix, iy;
int char_width;
int char_height;
int char_descender;
int capitalize, controlize;
static char string[MAXSTRINGLEN];

/*-----------------------------------------------------------------------*/
char *g_gets(stringcolor, backcolor, cursorcolor)
		   /* do a gets without using textports ..echoing the    */
		   /* character on the screen, erasing when necessary    */
/*-----------------------------------------------------------------------*/
Colorindex stringcolor, backcolor, cursorcolor;
{
  return g_getstring(stringcolor, backcolor, cursorcolor, "", MAXSTRINGLEN);
}


/*-----------------------------------------------------------------------*/
char *g_getstring(stringcolor,
		  backcolor,
		  cursorcolor,
		  protostring,
		  maxstringlen)
		   /* do a gets without using textports ..echoing the    */
		   /* character on the screen, erasing when necessary    */
/*-----------------------------------------------------------------------*/
Colorindex stringcolor, backcolor, cursorcolor;
char *protostring;	/* initial string */
int maxstringlen;
{
  return g_mode_getstring(stringcolor,
			  backcolor,
			  cursorcolor,
			  protostring,
			  maxstringlen,
			  PNL_TIM_NORMAL);  /* NORMAL is zero */
}

/*-----------------------------------------------------------------------*/
char *g_mode_getstring(stringcolor,
		       backcolor,
		       cursorcolor,
		       protostring,
		       maxstringlen,
		       mode)
		   /* do a gets without using textports ..echoing the    */
		   /* character on the screen, erasing when necessary    */

 /* mode: at this point this controls whether mouse events can terminate */
 /* reading.  mode = 0 implies Normal mode, mouse clicks terminate read */
 /* 	      and are requeued for the library */
 /* 	      mode = 1 implies terminate only on enter key: ignore mouse, */
 /* 	      mouse clicks consumed and */
 /* 	      cause the keyboard bell to ring if pnl_panel_bell is set */


/*-----------------------------------------------------------------------*/
Colorindex stringcolor, backcolor, cursorcolor;
char *protostring;	/* initial string */
int maxstringlen;
int mode;
{
    int g_getchar();
    register int i;
    int c;
    int protolen = strlen(protostring);

    if (mode&PNL_TIM_TERM_ENTER) termenter=TRUE;
    else			 termenter=FALSE;

    char_width  = strwidth("a");
    char_height  = getheight();
    char_descender = getdescender();
    getcpos(&ix, &iy);

    numqueued = 0;
    numevents = 0;
    capitalize = 0;
    controlize = 0;

    saveq();
    pushviewport();
    pushattributes();
    pushmatrix();
    screenspace();

    frontbuffer(1); backbuffer(0);

    for (i = 1; i <= MAXKBDBUT; i++) qdevice(i);
    if (protolen>maxstringlen) goto exit;

restart:
    bzero(string, maxstringlen);
    color(cursorcolor);
    drawcursor(protolen, maxstringlen);
    if (protolen) {
	(void) strcpy(string, protostring);
	cmov2i(ix,iy);
	color(stringcolor);
	charstr(protostring);
    }
    for (i = protolen;;) {
	string[i]=(char)(c=g_getchar());
 	switch(c) {
	    case '\n':
	    case '\r':
	    case '\0':
	    case QUIT:
		string[i] = '\0';
		goto exit;
	    case RESET:
		color(backcolor);
		drawcursor(i, maxstringlen);
		cmov2i(ix,iy);
		charstr(string);
		cmov2i(ix,iy);
		protolen=0;
		goto restart;
	    case ERASE:
		if (i == 0) break;
		string[i--] = '\0';
		color(backcolor);
		drawcursor(i+1, maxstringlen);
		color(cursorcolor);
		drawcursor(i, maxstringlen);
	    	cmov2i(ix+ i*char_width , iy); 
		break;
	    default:
		if (!(i<maxstringlen)) {
		    ringbell();
		    break;
		}
		color(backcolor);
		drawcursor(i, maxstringlen);
		color(cursorcolor);
		drawcursor(i+1, maxstringlen);
	    	color(stringcolor);
		charstr(&string[i++]);
		break;
	}
    }
exit:
    for (i = 1; i <= MAXKBDBUT; i++) unqdevice(i);
    restoreq();
    popmatrix();
    popattributes();
    popviewport();
    return(string);
}

static int drawcursor(pos, max)
int pos, max;
{
    if (pos<max) {	/* normal */
	rectfi(ix+pos*char_width,
	       iy-char_descender,
	       ix+pos*char_width+char_width,
	       iy-char_descender+char_height);
    } else {		/* at end */
	rectfi(ix+(max-1)*char_width,
	       iy-char_descender,
	       ix+(max-1)*char_width+char_width,
	       iy-char_descender+char_height);
    }
}

/*-------------------------------------------------------------------------*/
static char lowerkey(dev) /* maps sgi keyboard device numbers to printable */
			  /* ascii characters. null for nonprintables      */ 
Device dev;
/*-------------------------------------------------------------------------*/
{
    register i;

    for (i = 0; keyboard[i].device_num != -1; i++)
	if(dev == keyboard[i].device_num) return(keyboard[i].lower);
    return('\0');
}

/*-------------------------------------------------------------------------*/
static char upperkey(dev) /* maps sgi keyboard device numbers to printable */
			  /* ascii characters. null for nonprintables	   */
Device dev;
/*-------------------------------------------------------------------------*/
{
    register i;

    for (i = 0; keyboard[i].device_num != -1; i++)
	if(dev == keyboard[i].device_num) return(keyboard[i].upper);
    return('\0');
}


/*-----------------------------------------------------------------------*/
static int g_getchar()   /* do a getchar without using textports	 */
/*-----------------------------------------------------------------------*/
{
    short val; 
    Device key;

    for(;;){
        key= qread(&val);
	switch (key) {

#ifdef IRIS_4D
	    case LEFTCTRLKEY:
	    case RIGHTCTRLKEY:	controlize = val; break;
#else  /* IRIS_4D */
	    case CTRLKEY:	controlize = val; break;
#endif /* IRIS_4D */

	    case LEFTSHIFTKEY:
	    case RIGHTSHIFTKEY: capitalize = val; break;

	    case DELKEY:
	    case BACKSPACEKEY:  if (val) return(ERASE);

	    case ESCKEY:	if (val) return(QUIT);

	    case LEFTMOUSE:	
	    case REDRAW:
	    case RIGHTMOUSE:
	    case MIDDLEMOUSE:
	        if (termenter) {
		  if (val&&pnl_panel_bell) ringbell();
		  break;
		} else {
		  saveevent(key,val);
		  if (val) return(QUIT);
		}
		break;

	    case INPUTCHANGE:
		saveevent(key,val);
		break;
	    case MOUSEX:
	    case MOUSEY:
	        if (termenter) {
		} else {
		  saveevent(key,val);
		}
	        break;
 	    default:
		if (controlize)
		switch (key) {
		    case UKEY:
			return RESET;
		    default:
			break;
		} else if (val){
		    if (capitalize) return(upperkey(key));
		    else return (lowerkey(key));
	 	}
	}
    }
}

/*------------------------------------------------------------------------*/
static saveq()  /* save the device numbers of all keyboard queued devices */
		/* and unqueue them					  */
/*------------------------------------------------------------------------*/
{
   register i;

   buf_state = getbuffer();
   for (i = 1; i <= MAXKBDBUT; i++)
       if (isqueued(i)) {
	   qstack[numqueued++] = i;
	   unqdevice(i);
   }
}

/*-----------------------------------------------------------------------*/
static restoreq()	/* requeue all devices that were queued 	 */
/*-----------------------------------------------------------------------*/
{
Device dev;
short data;

    if (buf_state & 1) backbuffer(1);
    else backbuffer(0);
    if (buf_state & 2) frontbuffer(1);
    else frontbuffer(0);
    while (qtest()) {
	dev=qread(&data);
	saveevent(dev,data);
    }
    while (numqueued) qdevice(qstack[--numqueued]);
    restoreevents();
}

static saveevent(dev,data)
Device dev;
short data;
{
    eventq[numevents].dev=dev;
    eventq[numevents].data=data;
    numevents++;
}

static restoreevents()
{
int i;    
    for (i=0;i<numevents;i++) qenter(eventq[i].dev,eventq[i].data);
}
