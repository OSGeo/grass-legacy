/*  %W%  %G%  */
#define MASK unsigned char

build(plane, planebuf, cellbuf, ncols, cellsize)
unsigned char *planebuf;
unsigned char *cellbuf;
{

    static MASK mask[] = {0200, 0100, 0040, 0020, 0010, 0004, 0002, 0001};

    register unsigned char *cb;
    register unsigned char *pb;
    MASK *planemask;
    MASK cellmask;

    register int i;
    register int n;

    cb = cellbuf + cellsize - plane / 8 - 1;
    cellmask = mask[7 - plane % 8];

    pb = planebuf;
    planemask = mask;
    n = 0;
    for (i = 0; i < ncols; i++, cb += cellsize) {
        if (*pb & *planemask++)
            *cb |= cellmask;
        if (++n == 8) {
            n = 0;
            pb++;
            planemask = mask;
        }
    }
}
