
/* Text.c - save text string into last_text buffer */

#include "htmlmap.h"

Text(text)
	char *text ;
{
    int len = strlen(text);

    if (len > last_text_len) {
        free (last_text);
        last_text = (char *) malloc(len+1);
        last_text_len = len;
    }
    strcpy(last_text, text);
}
