/* Functions: get_cursor, set_cursor
**
** Author: Paul W. Carlson		Jan. 1990
*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

get_cursor()
{
    unsigned char high_byte, low_byte;
    long cursor_address;
    struct port_io_arg curs;

    /* get the high byte of the cursor address */
    curs.args[0].dir = OUT_ON_PORT;
    curs.args[0].port = CRTC_IND_SEL;
    curs.args[0].data = 0x0E;
    curs.args[1].dir = IN_ON_PORT;
    curs.args[1].port = CRTC_IND_REG;
    curs.args[2].dir = OUT_ON_PORT;
    curs.args[2].port = 0x00;
    curs.args[2].data = 0x00;
    ioctl(vidfd, EGAIO, &curs);
    high_byte = curs.args[1].data;

    /* get the low byte of the cursor address */
    curs.args[0].data = 0x0F;
    ioctl(vidfd, EGAIO, &curs);
    low_byte = curs.args[1].data;

    /* compute the cursor row and column */
    cursor_address = 256 * high_byte + low_byte;
    cursor_r = cursor_address / 80 + 1;
    cursor_c = cursor_address % 80 + 1;
}


void set_cursor()
{
    char buf[20];

    sprintf(buf, "\033[%d;%dH", cursor_r, cursor_c);
    write(vidfd, buf, strlen(buf));
}
