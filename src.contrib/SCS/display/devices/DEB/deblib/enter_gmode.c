

/*---------- Function: enter_gmode ----------*/
#include <stdio.h>
#include <termio.h>
extern int wind_x1, wind_y1, wind_x2, wind_y2;
enter_gmode()
{
    struct termio tty_param, tty_in;
    int out, in;

    out = fileno(stdout);
    ioctl(out,TCGETA,&tty_param);	/* clear OPOST flag in termio */
    tty_param.c_oflag &= ~OPOST;	/*   structure to prevent "stray" */
    tty_param.c_cflag &= ~PARENB;
    tty_param.c_cflag |= CS8;
    ioctl(out,TCSETAW,&tty_param);	/*   <CR> or <LF> */
    in = fileno(stdin);
    ioctl(in,TCGETA,&tty_in);
    tty_in.c_lflag &= ~(ECHO | ECHOE | ECHOK | ECHONL);
    tty_param.c_cflag &= ~PARENB;
    tty_param.c_cflag |= CS8;
    tty_param.c_iflag &= ~IXANY;
    ioctl(in,TCSETAW,&tty_in);	
    fwrite("\033G", 1, 2, stdout);
    fflush(stdout);
    wind_x1 = wind_y1 = 0;
    wind_x2 = 639;
    wind_y2 = 399;
}
