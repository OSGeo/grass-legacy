
/* Text.c - save text string into last_text buffer */

#include <stdlib.h>
#include <string.h>
#include "htmlmap.h"
#include "driverlib.h"

int Text (char *text)
{
    int len = strlen(text);

    if (len > last_text_len) {
        free (last_text);
        last_text = (char *) G_malloc(len+1);
        last_text_len = len;
    }
    strcpy(last_text, text);

    return 0;
}
