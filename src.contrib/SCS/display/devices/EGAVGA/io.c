/* Functions: enter_gmode, exit_gmode, put_int, put_chr  P.W.C.  April 1990 */

#include <stdio.h>
#include <termio.h>

enter_gmode(gmode)
int gmode;
{
    struct termio tty_param, tty_in;
    int out, in;

    out = fileno(stdout);
    ioctl(out, TCGETA, &tty_param);
    tty_param.c_oflag &= ~OPOST;
    ioctl(out, TCSETAW, &tty_param);
    in = fileno(stdin);
    ioctl(in, TCGETA, &tty_in);
    tty_in.c_lflag &= ~(ECHO | ECHOE | ECHOK | ECHONL);
    ioctl(in, TCSETAW, &tty_in);	
    fwrite("\033G", 1, 2, stdout);
    put_int(gmode);
    fflush(stdout);
}



exit_gmode()
{
    struct termio tty_out, tty_in;
    int out, in;

    put_chr('Q');
    fflush(stdout);
    out = fileno(stdout);
    ioctl(out, TCGETA, &tty_out);
    tty_out.c_oflag |= OPOST;
    ioctl(out, TCSETAW, &tty_out);
    in = fileno(stdin);
    ioctl(in, TCGETA, &tty_in);
    tty_in.c_lflag |= (ECHO | ECHOE | ECHOK | ECHONL);
    ioctl(in, TCSETAW, &tty_in);
}



put_chr(c)
char c;
{
    putc(c, stdout);
    fflush(stdout);
}



put_int(num)
int num;
{
    unsigned char low_byte, hi_byte;

    low_byte = (num & 0x001f) | 0x20;
    hi_byte = ((num & 0x03e0) >> 5) | 0x20;
    putc(low_byte, stdout);
    putc(hi_byte, stdout);
    fflush(stdout);
}

