#include "P.h"
clear_colors()
{
    zero(YELLOW, sizeof (YELLOW));
    zero(CYAN, sizeof (CYAN));
    zero(MAGENTA, sizeof (MAGENTA));
}

output_colors()
{
/* colors go into temp files for now */
    write (cyanfd, CYAN[0], COLORBYTES);
    write (cyanfd, CYAN[1], COLORBYTES);
    write (magentafd, MAGENTA[0], COLORBYTES);
    write (magentafd, MAGENTA[1], COLORBYTES);
    write (yellowfd, YELLOW[0], COLORBYTES);
    write (yellowfd, YELLOW[1], COLORBYTES);
}

do_color (buf)
    char *buf;
{
    Poutc (GS);	/* send GRAPHICS START, then the color data */
    Pout (buf, COLORBYTES);
}
