#include <string.h>
#include "gis.h"
/*
 * squeeze - edit superfluous white space out of strings
 *
 *  char *G_squeeze (s)
 *     char *s;
 *
 * scan a string of text, converting tabs to spaces and
 * compressing out leading spaces, redundant internal spaces,
 * and trailing spaces.
 * returns the address of the resulting compressed string.
 *
 * last modification: 12 aug 81, j w hamilton
 *
 * 1998-04-04  WBH
 *     Also squeezes out newlines -- easier to use with fgets()
 *
 * 1999-19-12 Werner Droege 
 *     changed line 37, line 48ff. -- return (strip_NL(line))
 */

#include <ctype.h>


/*!
 * \brief remove unnecessary white space
 *
 * Leading and trailing white space is removed from the string <b>s</b> and internal
 * white space which is more than one character is reduced to a single space
 * character. White space here means spaces, tabs, linefeeds, newlines, and
 * formfeeds. Returns <b>s.</b>
 *
 *  \param s
 *  \return char * 
 */

char *G_squeeze (char *line)
{
    register char *f = line, *t = line;
    int l;

    /* skip over space at the beginning of the line. */
    while (isspace (*f))
        f++;

    while (*f)
        if (! isspace (*f))
            *t++ = *f++;
        else
            if (*++f)
                if (! isspace (*f))
                    *t++ = ' ';
    *t = '\0';
    l=strlen(line)-1;
    if(*(line+l)=='\n') *(line+l)='\0';
    return line;
}
