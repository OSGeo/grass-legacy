static char ID[] = "@(#) e00b2a version 1.1 (C) M. Wurtz 2/12/98";

/***************************************************************

  E00b2a converts Arc/Info E00 export files from Binary to Ascii
 
  **************************************************************

  This file may be distributed under the GNU Public License, version 2, or
  any higher version.  See section COPYING of the GNU Public license
  for conditions under which this file may be redistributed.
   
  Only numbers and spaces are compressed. Linefeed are also coded, so
  each line has the same lenght.  Only the first line is padded with blank
  and begin with EXP 1 instead of EXP 0 (ascii export files)
  
  Compression is done as following (at least as far as I guess) :

  Escape character is tilde : '~'

   "~~" means ~

   "~-" means -

   "~-" means E

   "~}" means LineFeed

   "~ x", where x is an ASCII char beetween '!' and (don't know max) means
        n spaces, with n = x - ' ', (so n=1 when x == '!', etc...)

   "~!" intoduce a integer (even number of digits)

   "~N" intoduce a integer (odd number of digits)

   '~' followed by '"', '%', '(', 'O', 'P' or 'T' is for fixed decimal numbers

   "~" followed by '@', '^', '0', '1' is for float (scientific notation)

  For the two last types of data, there is probably other possibilities,
  because number can be split anywhere between the first digit and the
  last one, but I have no examples yet... so wait for new releases...

  Any other combination is copied to output without change.

  Digit coding : Each char codes two digit, from 00 ('!') to 91 ('|')
   for 92 to 99, there is two char :  '}' followed by '!' to '('
   the number ends when a space or a tilde is encountered
   the program expects an ascii representation of chars

  A good way to improve this is to send me a "diff" file (correctly translated
  and the result given by this program if some "~..." strings are not
  translated.  You can use the web translator (based on Arc/Info) for a correct
  translation at the url "http://www.geo.ed.ac.uk/~anp/gms/e00.htm"
  Results must be the same (except some name strings).

  History :
   - v 1.1  (2/12/98)
	    added "~%...~" recognition. (number ended by a dot)

   - v 1.0  (11/11/98) version 1.0 only a filter, no arguments
            float and decimal coding gessed
	    no test for garbage input except first line...
            key recognized  ~ - <space> ! } N " O P T ^ 0 1 @ E ] B b

***********************************************************************/

#include <stdio.h>
#include <strings.h>

long line_in;	/* line number on input */
long line_out;	/* line number on output */

/* ignore return or newline even inside escape sequences */
/* ignore also MS-DOS end of file ('^Z') when found      */

int mygetchar()
{
    int ch;
    while ((ch = getchar()) != EOF) {
	if (ch == 26)
	    continue;
	if (ch != '\n' && ch != '\r')
	    return ch;
	else if (ch == '\n')
	    line_in++;
    }
    return ch;
}

/* read a number (return first space or tilde in the stream) */

int getnum( char *s) {
    char *p;		/* travelling in string... */
    int ch;		/* for char reading        */

    p = s;
    while ((ch = mygetchar()) != ' ' && ch != '~' && ch != EOF) {
	if (ch == '}')
	    ch += (mygetchar() - '!');
	*p++ = (((ch-'!') / 10) + '0');
	*p++ = (((ch-'!') % 10) + '0');
    }
    *p = 0;
    return ch;
}


void main()
{
    int i, c;		/* for loop and char reading */
    int d, n, m;	/* decimal point position, number of digits */
    int flag = 0;	/* 1 when inside an escape sequence */
    char s[100], *p;	/* temp string for numbers */
    char sign;		/* exponent sign */

#ifndef DEBUG	/* When debugging, we want to look at partial files */
		/* so we ignore the test on first line and go on    */

    fgets( s, 81, stdin);			/** header "EXP 1" **/
    if (s[0] != 'E' || s[1] != 'X' || s[2] != 'P' || s[5] != '1') {
	fprintf( stderr, "Not an Arc/Info binary export file\n");
	exit( 1);
    }
    s[5] = '0';					/* becomes "EXP 0" */
    p = s + strlen(s) - 1;
    while (*p == ' ' || *p == '\n' || *p == '\r') {
	p--;					/* remove trailing spaces */
	continue;
    }
    *++p = 0;
    puts( s);
    line_in = line_out = 2;
#else
    line_in = line_out = 1;
#endif

    while ((c=mygetchar()) != EOF) {
	if (flag) {
	    switch (c) {
		case '}' :  putchar( '\n');		/** newline **/
			    line_out++;
			    break;
		case ' ' :  i = mygetchar() - ' ';	/** spaces **/
			    while (i--)
				putchar( ' ');
			    break;
							/** integers **/
		case '!' :  while ((c = mygetchar()) != ' ' && c != '~') {
				if (c == '}')
				    c += (mygetchar() - '!');
				putchar( ((c-'!') / 10) + '0');
				putchar( ((c-'!') % 10) + '0');
			    }
			    if (c == ' ')	/* stops when space or  */
				putchar( c);	/* tilde.  In this case */
			    else		/* stay in escape mode  */
				flag++;
			    break;
		case 'N' :  c = getnum( s);
			    i = strlen( s) - 1;
			    s[i] = 0;		/* the last digit is to */
			    for (p=s; *p; p++)	/* thrown away...       */
				putchar( *p);
                            if (c == ' ')
                                putchar( c);
                            else
                                flag++;
                            break;

						/** fixed decimal numbers **/
		case '%' :
		case '"' :
		case '(' :  n = c - '!';	/* # digit before point */
			    c = getnum( s); p = s;
			    m = strlen( s) - n;
			    while (n--)
				putchar( *p++);
			    putchar( '.');
			    if (m == 0 && c == '~') {
				c = mygetchar();
				putchar( c);
			    } else {
				while (m--)
				    putchar( *p++);
				if (c == ' ')
				    putchar( c);
				else
				    flag++;
			    }
			    break;

		case 'O' :
		case 'P' :  i = c - '!';
			    n = (i%10) - (i/10);
                            c = getnum( s); p = s;
                            m = strlen( s) - n; n--;
                            while (n--)
                                putchar( *p++);
                            putchar( '.');
                            while (m--)
                                putchar( *p++);
                            if (c == ' ')
                                putchar( c);
                            else
                                flag++;
                            break;

		case 'T' :  i = c - '!';	/* to be confirmed */
                            n = (i%10) + (i/10);
                            c = getnum( s); p = s;
                            m = strlen( s) - n - 1;
                            while (n--)
                                putchar( *p++);
                            putchar( '.');
                            while (m--)
                                putchar( *p++);
                            if (c == ' ')
                                putchar( c);
                            else
                                flag++;
                            break;

						/** float numbers **/
/* n : 1 if one digit before dot and dot, 0 if no dot */
/* d : 2 if even number of digits, 3 if odd number of digits */
/* sign : exponent sign */

		case '0' :  n = 0; d = 2; sign = '+';
			    goto write_num;
/*** missing case        :  n = 0; d = 2; sign = '-';          missing case ***/
			    goto write_num;
		case ']' :  n = 0; d = 3; sign = '+';
			    goto write_num;
/*** missing case        :  n = 0; d = 3; sign = '-';          missing case ***/
			    goto write_num;
		case '1' :  n = 1; d = 2; sign = '+';
			    goto write_num;
		case '@' :  n = 1; d = 2; sign = '-';
			    goto write_num;
		case '^' :  n = 1; d = 3; sign = '+';
			    goto write_num;
/*** missing case        :  n = 1; d = 3; sign = '-';          missing case ***/
write_num:                  c = getnum( s); p = s;
                            m = strlen( s) - n - d;
                            if (n) {
				while (n--)
                                    putchar( *p++);
				putchar( '.');
			    }
			    while (m--)
                                putchar( *p++);
                            putchar( 'E');
			    putchar( sign);
			    d = 2;
                            while (d--)
                                putchar( *p++);
                            if (c == ' ')
                                putchar( c);
                            else
                                flag++;
                            break;

		case 'E' :				/** exponent **/
		case 'B' :
		case 'b' :
		case '~' :  				/** tilde **/
		case '-' :  putchar( c);		/** minus **/
			    break;
		default :   putchar( '~'); putchar( c);	/** unknown escape sequence **/
			    fprintf( stderr,
				     "Unknow code \"~%c\" line_in %ld, line_out %d\n",
				     c, line_in, line_out);
	    }
	    flag--;
	} else {
	    if (c == '~') {
		flag = 1;
	    } else {
		putchar( c);
	    }
	}
    }
}
