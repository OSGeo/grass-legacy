

/*---------- Function: exit_gmode ----------*/
#include <stdio.h>
#include <termio.h>
exit_gmode()
{
    struct termio tty_out, tty_in;
    int out, in;

    put_chr('Q');
    fflush(stdout);
    out = fileno(stdout);
    ioctl(out,TCGETA,&tty_out);		/* turn opost flag in termio */
    tty_out.c_oflag |= OPOST;		/*   back on */
    ioctl(out,TCSETAW,&tty_out);
    in = fileno(stdin);
    ioctl(in,TCGETA,&tty_in);
    tty_in.c_lflag |= (ECHO | ECHOE | ECHOK | ECHONL);
    ioctl(in,TCSETAW,&tty_in);
}
