/*---------- Function: unblock_mode ----------*/
#include <stdio.h>
#include <termio.h>
static struct termio blocked_mode, norm_mode;
static int normal = 1;
extern FILE *debug;

unblock_mode()
{
    if (!normal)
        return(0);
    if (ioctl(0, TCGETA, &norm_mode) == -1)
    {   fprintf(stderr, "Can't get terminal settings\n");
        fprintf(debug,"ioctl error in unblock_mode - can't get settings\n");
        exit(1);
    }
    ioctl(0, TCGETA, &blocked_mode);
    blocked_mode.c_lflag &= ~ICANON;
    blocked_mode.c_cc[VMIN] = 1;
    blocked_mode.c_cc[VTIME] = 0;
    if (ioctl(0, TCSETA, &blocked_mode) == -1)
    {   fprintf(stderr, "Can't change terminal settings\n");
        fprintf(debug,"ioctl error in unblock_mode - can't change settings\n");
        exit(1);
    }
    normal = 0;
}

/*---------- Function: normal_mode ----------*/
normal_mode()
{
    if (normal)
        return(0);
    if (ioctl(0, TCSETA, &norm_mode) == -1)
    {   fprintf(stderr, "Can't restore terminal settings\n");
        fprintf(debug,"ioctl error in unblock_mode - can't restore settings\n");
        exit(1);
    }
    normal = 1;
}
