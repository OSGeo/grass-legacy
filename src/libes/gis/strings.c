#include "gis.h"
/*
 * string/chring movement functions
 *
** G_strcpy (T, F)
** G_strncpy (T, F, n)	copy F up to null or n, always copy null
** G_chrcpy (T, F, n)
** G_strmov (T, F)
** G_chrmov (T, F, n)
** G_strcat (T, F)
** G_chrcat (T, F, n)
**     char *T, *F;
**     int n;
 *
 * G_strcpy (T, F)    copy F up to null, copy null
 * G_chrcpy (T, F, n) copy F up to n,    copy null
 * 
 * G_strmov (T, F)    copy F up to null
 * G_chrmov (T, F, n) copy F up to n
 * 
 * G_strcat (T, F)    cat F up to null, copy null
 * G_chrcat (T, F, n) cat F up to n,    copy null
 *
 * the -cpy and -cat functions are for null-terminated destinations;
 * the -mov functions are for non-null-terminated ('chring') destinations.
 * all functions return 'T'.
 *
 * Author Dave Gerdes (USACERL)
 *
 *
 * G_strcasecmp(a, b) char *a, *b;
 *   string compare ignoring case (upper or lower)
 *   returns: -1 a<b; 0 a==b; 1 a>b
 *
 * Author Michael Shapiro (USACERL)
 *
 *
 * G_strstr(mainString, subString)
 *	Return a pointer to the first occurrence of subString
 *	in mainString, or NULL if no occurrences are found.
 * G_strdup(string)
 *	Return a pointer to a string that is a duplicate of the string
 *	given to G_strdup.  The duplicate is created using malloc.
 *	If unable to allocate the required space, NULL is returned.
 *
 * Author: Amit Parghi (USACERL), 1993 02 23
 *
 * G_strchg(char* bug, char character, char new) {
 *      replace all occurencies of character in string(inplace) with new
 *
 * Author: Bernhard Reiter (Intevation GmbH, Germany)
 */

static char *G_strend (register char *S)
{
    while (*S)
	S++;
    return (S);
}

char *G_strcpy (register char *T,register char *F)
{
    register char *d = T;

    while (*d++ = *F++)
        ;
    return (T);
}

char *G_chrcpy (
    register char *T,register char *F,
    register int n)
{
    register char *d = T;

    while (n--)
        *d++ = *F++;
    *d = '\0';
    return (T);
}

char *G_strncpy (
    register char *T,register char *F,
    register int n)
{
    register char *d = T;

    while (n-- && *F)
        *d++ = *F++;
    *d = '\0';
    return (T);
}

char *G_strmov (
    register char *T,register char *F)
{
    register char *d = T;

    while (*F)
        *d++ = *F++;
    return (T);
}

char *G_chrmov (
    register char *T,register char *F,
    register int n)
{
    register char *d = T;

    while (n--)
        *d++ = *F++;
    return (T);
}

char *G_strcat (
    register char *T,register char *F)
{
    G_strcpy (G_strend (T), F);
    return (T);
}

char *G_chrcat (
    register char *T,register char *F,
    register int n)
{
    G_chrcpy (G_strend (T), F, n);
    return (T);
}

int G_strcasecmp(char *x,char *y)
{
    int xx,yy;

    if (!x)
	return y ? -1 : 0;
    if (!y)
	return x ? 1 : 0;
    while (*x && *y)
    {
	xx = *x++;
	yy = *y++;
	if (xx >= 'A' && xx <= 'Z')
	    xx = xx + 'a' - 'A';
	if (yy >= 'A' && yy <= 'Z')
	    yy = yy + 'a' - 'A';
	if (xx < yy) return -1;
	if (xx > yy) return 1;
    }
    if (*x) return 1;
    if (*y) return -1;
    return 0;
}



#include <sys/types.h>
#include <string.h>
#include <stdlib.h>

#ifndef NULL
#define NULL		0
#endif


char *G_strstr(
    char *mainString,
    char *subString)
{
    char *p, *q;
    int length;

    p = subString;
    q = mainString;
    length = strlen(subString);

    do {
	while (*q != '\0' && *q != *p) {	/* match 1st subString char */
	    q++;
	}
    } while (*q != '\0' && strncmp(p, q, length) != 0 && q++);
				/* Short-circuit evaluation is your friend */

    if (*q == '\0') {				/* ran off end of mainString */
	return NULL;
    } else {
	return q;
    }
}


char *G_strdup(char *string)
{
    char *p;

    p = malloc(strlen(string) + 1);

    if (p != NULL) {
	strcpy(p, string);
    }

    return p;
}


char * G_strchg(char* bug, char character, char new) {
 /* replace all occurencies of character in string(inplace) with
  * new */

 char *help = bug;
 while(*help) {
	if (*help==character)
		*help=new;
	help++;
	}
 return bug;
}
