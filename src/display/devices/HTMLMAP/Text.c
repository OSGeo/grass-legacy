
/* Text.c - save text string into last_text buffer */

#include <stdlib.h>
#include <string.h>
#include "htmlmap.h"
#include "driverlib.h"

int Text (char *text)
{
    int len = strlen(text);
    char *d, *s;

    if (len > last_text_len) {
        free (last_text);
        last_text = (char *) malloc(len+1);
        last_text_len = len;
    }

    /* copy string to last_text area, make sure we don't copy \n */
    for (d = last_text, s = text; *s != '\0'; s++) {
        if (*s != '\n') {
	    *d = *s;
	    d++;
        }
    }
    *d = '\0';

    return 0;
}
