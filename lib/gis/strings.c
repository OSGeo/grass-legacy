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
 *
 * char * G_str_replace(char* buffer, char* old_str, char* new_str)
 *  Replace all occurencies of old_str in buffer with new_str
 *  Author Beverly Wallace (LMCO)
 */

#include <string.h>
#include <stdlib.h>
#include "gis.h"

static char *G_strend (char *S)
{
    while (*S)
	S++;
    return (S);
}

char *G_strcpy (char *T, const char *F)
{
    char *d = T;

    while ((*d++ = *F++))
        ;
    return (T);
}

char *G_chrcpy (char *T, const char *F, int n)
{
    char *d = T;

    while (n--)
        *d++ = *F++;
    *d = '\0';
    return (T);
}

char *G_strncpy (char *T, const char *F, int n)
{
    char *d = T;

    while (n-- && *F)
        *d++ = *F++;
    *d = '\0';
    return (T);
}

char *G_strmov (char *T, const char *F)
{
    char *d = T;

    while (*F)
        *d++ = *F++;
    return (T);
}

char *G_chrmov (char *T, const char *F, int n)
{
    char *d = T;

    while (n--)
        *d++ = *F++;
    return (T);
}

char *G_strcat (char *T, const char *F)
{
    G_strcpy (G_strend (T), F);
    return (T);
}

char *G_chrcat (char *T, const char *F, int n)
{
    G_chrcpy (G_strend (T), F, n);
    return (T);
}

int G_strcasecmp(const char *x, const char *y)
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


char *G_strstr(char *mainString, const char *subString)
{
    const char *p;
    char *q;
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


char *G_strdup(const char *string)
{
    char *p;

    p = malloc(strlen(string) + 1);

    if (p != NULL) {
	strcpy(p, string);
    }

    return p;
}


char *G_strchg(char* bug, char character, char new) {
 /* replace all occurencies of "character" in string(bug) with
  * "new", returns new string */

 char *help = bug;
 while(*help) {
	if (*help==character)
		*help=new;
	help++;
	}
 return bug;
}

/*--------------------------------------------------------------------
  \brief Replace all occurencies of old_str in buffer with new_str.
  \return Returns the newly allocated string, input buffer is unchanged 
  
   Author Beverly Wallace (LMCO) 3/11/04, slightly modified RB/MN
 
  Code example:
  \verbatim
      char *name;
      name = G_str_replace ( inbuf, ".exe", "" );
      ... 
      free(name);
  \endverbatim
--------------------------------------------------------------------*/
char *G_str_replace(char* buffer, const char* old_str, const char* new_str) 
{

	char *B, *R;
	const char *N;
	char *replace;
	int count, len;

	/* Make sure old_str and new_str are not NULL */
	if (old_str == NULL || new_str == NULL)
		return G_strdup (buffer);
	/* Make sure buffer is not NULL */
	if (buffer == NULL)
		return NULL;

	/* Make sure old_str occurs */
  	B = strstr (buffer, old_str);
	if (B == NULL)
		/* return NULL; */
		return G_strdup (buffer);

	if (strlen (new_str) > strlen (old_str)) {
		/* Count occurences of old_str */
		count = 0;
		len = strlen (old_str);
		B = buffer;
		while(B != NULL && *B != '\0') {
  			B = G_strstr (B, old_str);
			if (B != NULL) {
				B += len;
				count++;
			}
		}
	
  		len = count * (strlen(new_str) - strlen(old_str)) 
			+ strlen(buffer);

	}
	else 
		len = strlen(buffer);

	/* Allocate new replacement */
	replace = G_malloc (len + 1);
	if (replace == NULL)
		return NULL;

	/* Replace old_str with new_str */
	B = buffer;
	R = replace;
	len = strlen (old_str);
	while(*B != '\0') {
		if (*B == old_str[0] && strncmp (B, old_str, len) == 0) {
			N = new_str;
			while (*N != '\0')
				*R++ = *N++;
			B += len;
		}
		else {
			*R++ = *B++;
		}
	}
	*R='\0';

	return replace;
}
