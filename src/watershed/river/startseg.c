#define EXTERN extern
#include "global.h"

startseg(name, seg, len)

    char *name;
    SEGMENT *seg;

{

    int fd;

    fd = creat(name,0666);
    segment_format(fd, nrows, ncols, 10, 10, len);
    close(fd);

    fd = open(name,2);
    segment_init(seg, fd, 4);

    return fd;
}
