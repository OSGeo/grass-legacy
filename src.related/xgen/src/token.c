/**********************************************************************
   token.c      - tokenize a string and return an array of tokens
 *********************************************************************/
/*******************************************************************************
Xgen was developed by Kurt Buehler, while at the Center for Advanced Decision
Support for Water and Environmental Systems (CADSWES), University of Colorado
at Boulder and at the Indiana Water Resources Research Center (IWRRC),
Purdue University for the U.S. Army Construction Engineering Research
Laboratory in support of the Geographical Resources Analysis Support
System (GRASS) software. The example scripts were developed by Ms. Christine
Poulsen of USA-CERL, much thanks goes to her for her work.

Permission to use, copy, modify and distribute without charge this software,
documentation, etc. is granted, provided that this comment is retained,
and that the names of Kurt Buehler, Christine Poulsen, CADSWES, IWRRC,
the University of Colorado at Boulder, Purdue University, or USA-CERL are not
used in advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

The author disclaims all warranties with regard to this software, including
all implied warranties of merchantability and fitness, in no event shall
the author be liable for any special, indirect or consequential damages or
any damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of this
software.
*******************************************************************************/
/*
 * This code taken from GRASS source code. Written by Michael Shapiro and
 * Dave Gerdes wht it does:break buf into tokens. delimiters are replaced by
 * NULLs and tokens array will point to varous locations in buf buf must not
 * have a new line
 * 
 * tokens = Tokenize (string, delimstr); FreeTokens (tokens);
 * 
 * 
 * NOTE FreeTokens() must be called when you are finished with tokens to release
 * the memory
 */
/*
 * Given buf,  turn delimiters in '\0'  and place pointers to tokens in
 * tokens.
 */
#include "xgen.h"

char                          **
Tokenize(buf, delim)
    char                           *buf;
    char                           *delim;
{
    int                             i;
    char                          **tokens;
    char                           *tmp;

    i = 0;
    while (*buf == ' ' || *buf == '\t') /* needed for free () */
        buf++;

    tmp = XtMalloc(strlen(buf) + 1);
    strcpy(tmp, buf);
    buf = tmp;

    tokens = (char **) XtMalloc(sizeof(char *));

    while (1) {
        while (*buf == ' ' || *buf == '\t')
            buf++;
        if (*buf == 0)
            break;
        tokens[i++] = buf;
        tokens = (char **) XtRealloc((char *) tokens, (i + 1) * sizeof(char *));

        while (*buf && (strchr(delim, *buf) == NULL))
            buf++;
        if (*buf == 0)
            break;
        *buf++ = 0;
    }
    tokens[i] = NULL;

    return (tokens);
}

int
FreeTokens(tokens)
    char                          **tokens;
{
    if (tokens[0] != NULL)
        XtFree((char *) tokens[0]);
    XtFree((char *) tokens);
    return (0);
}

int
NumberOfTokens(tokens)
    char                          **tokens;
{
    int                             n;
    for (n = 0; tokens[n]; n++);
    return n;
}
