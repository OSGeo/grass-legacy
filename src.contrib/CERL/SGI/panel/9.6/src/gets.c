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
 */
#include <stdio.h>
#include <gl.h>
#include <device.h>

#define MAXSTRINGLEN 	80
#define MAXQUEUED 	100
#define MAXDEVICES 	1024
#define ERASE   	-2
#define QUIT		-1

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


static char string[MAXSTRINGLEN];
static int bufmode;

/*-----------------------------------------------------------------------*/
char *g_gets(ix, iy, stringcolor, backcolor)
		   /* do a gets without using textports ..echoing the    */
		   /* character on the screen, erasing when necesary     */
/*-----------------------------------------------------------------------*/
Colorindex stringcolor, backcolor;
{

    static int inited = 0;
    char g_getchar();
    int i;
    int char_width = strwidth("a");

    saveq();
    frontbuffer(TRUE);
    backbuffer(FALSE);

    for (i = 1; i <= MAXKBDBUT; i++) qdevice(i);
    cmov2i(ix, iy);
    bzero(string, MAXSTRINGLEN);

    for (i = 0;  i < MAXSTRINGLEN;) {
 	switch(string[i] = g_getchar())
	{
	    case '\n':
	    case '\r':
/* checkit(); */
		string[i] = '\0';
		goto exit;

	    case '\0':
	    case QUIT:
		for (i = 1; i <= MAXKBDBUT; i++) unqdevice(i);
		restoreq();
		return(NULL);

	    case ERASE:
		if (i == 0) break;
		string[i--] = '\0';
	    	cmov2i(ix+ i*char_width , iy); 
	        color(backcolor);
		charstr(&string[i]);
	    	cmov2i(ix+ i*char_width , iy); 
		break;

	    default:
/* checkit(); */
	    	color(stringcolor);
		charstr(&string[i++]);
		break;
	}
    }

exit:
    for (i = 1; i <= MAXKBDBUT; i++) unqdevice(i);
    restoreq();
    return(string);
}

/*-----------------------------------------------------------------------*/
static char g_getchar()   /* do a getchar without using textports	 */
/*-----------------------------------------------------------------------*/
{
    static capitalize = 0;
    short val; 
    char upperkey(), lowerkey();
    long key;

    for(;;){
	key= qread(&val);
	if (key == LEFTSHIFTKEY || key == RIGHTSHIFTKEY){
	    capitalize = val;
	    continue;
	}else if (val)	/* do until user holds a key down */
	    break;
    }

    switch(key)
    {
	case DELKEY:
	case BACKSPACEKEY:
	    return(ERASE);

	case ESCKEY:
	    return(QUIT);
 	default:
	    if (capitalize) return(upperkey(key));
	    else return (lowerkey(key));
    }
}

/*-------------------------------------------------------------------------*/
static char lowerkey(dev) /* maps sgi keyboard device numbers to printable */
			  /* ascii characters. null for nonprintables      */ 
/*-------------------------------------------------------------------------*/
{
    int i;

    for (i = 0; keyboard[i].device_num != -1; i++)
	if(dev == keyboard[i].device_num) return(keyboard[i].lower);
    return('\0');
}

/*-------------------------------------------------------------------------*/
static char upperkey(dev) /* maps sgi keyboard device numbers to printable */
			  /* ascii characters. null for nonprintables	   */
/*-------------------------------------------------------------------------*/
{
    int i;

    for (i = 0; keyboard[i].device_num != -1; i++)
	if(dev == keyboard[i].device_num) return(keyboard[i].upper);
    return('\0');
}


static int numqueued = 0;
static int qstack[MAXQUEUED];

/*------------------------------------------------------------------------*/
static saveq()  /* save the device numbers of all keyboard queued devices */
		/* and unqueue them					  */
/*------------------------------------------------------------------------*/
{
   int i;

   bufmode = getbuffer();   /* also save enabled buffers */
   for (i = 1; i <= MAXKBDBUT; i++)
       if (isqueued(i)) {
	   qstack[numqueued++] = i;
	if (numqueued >= MAXQUEUED) {
	   (void) fprintf(stderr, "Eeeek qstack overflow\n");
	   return;
 	}
        unqdevice(i);
   }
}

/*-----------------------------------------------------------------------*/
static restoreq()	/* requeue all devices that were queued 	 */
/*-----------------------------------------------------------------------*/
{
    backbuffer(bufmode&0x1);	/* also restore enabled buffers */
    frontbuffer(bufmode&0x2);
    while (numqueued) qdevice(--numqueued);
    qreset();
}
