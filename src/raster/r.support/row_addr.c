#include "config.h"

#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

int next_row_addr (int fd, off_t *offset, int nbytes)
{
    unsigned char buf[256];
    int i;

/* nbytes <=0 means pre 3.0 compression */
    if (nbytes <= 0)
	return (read (fd, offset, sizeof(*offset)) == sizeof(*offset)) ;

/* 3.0 compression */
    if (read (fd, buf, nbytes) != nbytes)
	return 0;
    *offset = 0;
    for (i = 0; i < nbytes; i++)
	*offset = *offset * 256 + buf[i];
    return 1;
}
