/*******************************************************************
** Read a line in an EOO file.  If the file is not compressed,
** just call fgets.  Else decode the input stream.
**
** Decoding is based on an obsolete program I wrote (e00b2a)
** and some hints about the coding rule that Daniel Morisette
** (danmo@videotron.ca) mailed me (I used first a case by case logic)
** Look at http://pages.infinit.net/danmo/e00/index.html for his
** unembeded e00 compression/decompression utility.
**
********************************************************************/
#include <stdio.h>
#include <string.h>
#include "gis.h"

extern int compressed;	/* 1 if compressed, 0 if not */
extern FILE *fde00;

static long line_in = 2;/* line number in input file */

static int nchar;	/* number of output characters */
static char *p;		/* shared pointer for line filling */

/* Add a char to output line. Verify that output line length is <= 80 */

void addchar( char c)
{
    char msg[80];

    if (nchar >= 80) {
	sprintf( msg, "Line too long, input file corrupted near line %ld\n",
		line_in);
	G_fatal_error( msg);
    }
    *p++ = c;
    nchar++;
}

/* Read a character on compressed input file */
/* ignore return or newline and also MS-DOS end of file ('^Z') */

int mygetchar(void)
{
    int ch;
    while ((ch = fgetc( fde00)) != EOF) {
        if (ch == 26)	/* ^Z */
            continue;
        if (ch != '\n' && ch != '\r')
            return ch;
        else if (ch == '\n')
            line_in++;
    }
/* Here, we should introduce some code to read the next */
/* file in a multipart e00 file (.e00, .e01, .e02, ...) */
    G_fatal_error( "End of file unexpected");
    return ch;		/* just to keep compiler silent */
}

/***************************************************************

Compression Hints :

  Only numbers and spaces are compressed. Linefeed are also coded, so
  each line has the same lenght.  Only the first line is padded with blank
  and begin with EXP 1 instead of EXP 0 (ascii export files)

  Compression is done as following (at least as far as I guess) :

  Escape character is tilde : '~'

   "~~" means ~

   "~}" means LineFeed

   "~ x", where x is an ASCII char beetween '!' and (don't know max) means
        n spaces, with n = x - ' ', (so n=1 when x == '!', etc...)

  Other codes introduce numbers. The first character defines
  decimal point position, odd or even number of digits and exponent.
  Unexpected combinations are copied to output without change.

  Digit coding : Each char codes two digit, from 00 ('!') to 91 ('|')
   for 92 to 99, there is two char :  '}' followed by '!' to '('
   the number ends when a space or a tilde is encountered (in this
   case the next character is printed, except if it is a space or a '{',
   in which case compression rules apply).
   the program expects an ascii representation of chars

********************************************************************/

long read_e00_line( char *line)
{
    int i, c;           /* for loop and char reading */
    int d, n, m;        /* decimal point position, number of digits */
    int flag = 0;       /* 1 when inside an escape sequence */
    char s[64], *q;     /* temp string for numbers */
    char sign;          /* exponent sign */
    char msg[100];	/* error message */


    if (!compressed) {
	if (fgets( line, 84, fde00) == NULL)
	    G_fatal_error( "End of file unexpected");
	for (p = line; *p; p++)
	    if (*p == '\n' || *p == '\r') {
		*p = 0;
		break;
	    }
	return ftell( fde00);
    } else {
	flag = 0;		/* We start in nocompress mode, and */
	nchar = 0;		/* output line is empty             */
	p = line;
	for (;;) {
	    c=mygetchar();
            if (flag) {
              switch (c) {
		case '}' :  *p = 0;			/** end of line **/
			    return ftell( fde00);
			    break;
		case ' ' :  i = mygetchar() - ' ';  /** spaces **/
			    while (i--)
				addchar( ' ');
			    break;
		case '~' :  addchar( c);            /** tilde **/
			    break;			/* just a copy */
		default  :  n = c - '!';
			    if (n < 0 || n >=90) {
				sprintf( msg, "Unknown code \"~%c\" line %ld\n",
					c, line_in);
	    			G_fatal_error( msg);
			    }
			    d = n % 15;		/* # digit before point */
			    i = n / 45;		/* 1 if odd # of digits */
			    n /= 15;		/* exp if n%3 is 1 or 2 */
			    q = s;
			    while ((c = mygetchar()) != ' ' && c != '~') {
				if (c == '}')
				    c += (mygetchar() - '!');
				*q++ = (((c-'!') / 10) + '0');
				*q++ = (((c-'!') % 10) + '0');
			    }
			    *q = 0; q = s;
			    m = strlen( s) - i - d;
			    if (i = n % 3)	/* i==1 : E+, i==2 : E- */
				m -= 2;		/* i==0 => no exponent  */
			    if (d) {		/* digits before DP     */
				while (d--)
				    addchar( *q++);
				addchar( '.');
			    }
			    if (m < 0) {	/* digits after or without DP */
				sprintf( msg,
				"Input file corrupted : \"%s\" near line %ld\n",
				s, line_in);
	    			G_fatal_error( msg);
			    }
			    if (m > 0)
				while (m--)
				    addchar( *q++);
			    if (i) {		/* allways 2 digits after exp */
				addchar( 'E');
				addchar( i==1 ? '+' : '-');
				addchar (*q++);
				addchar (*q++);
			    }
			    if (c == '~') {	/* may end a numeric sequence */
				c = mygetchar();
				if (c == '}') {	/* when end of line reached */
				    *p = 0;
				    return ftell( fde00);
				    break;
				}
				if (c == ' ') { /* or spaces */
				    i = mygetchar() - ' ';
				    while (i--)
					addchar( c);
				} else {	/* or something else ? */
				    addchar( c);
				}
				break;
			    } else
				addchar( c);
	      }
	      flag = 0;
	    } else {
		if (c == '~') {
		    flag = 1;
		} else {
	 	    addchar( c);
		}
	    }
	}
    }
}
