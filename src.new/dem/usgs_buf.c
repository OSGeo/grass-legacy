/* %W% %G% */
#include "usgs.h"

get_buf()
{
    int n_read;

    buffer = buf_start;
    if (fgets(buffer, RECSIZE, tapefile) == NULL) {
        fprintf(stderr, "get_buf: error reading file");
        return(0);
    }
    n_read = strlen(buffer);
    buf_end = buffer + (n_read);
    return(n_read);
}


skip_file()
{
    int status;

    do {
        status = get_buf();
    } while (status);
}

