/*---------- Function: raw_tty ----------*/
#include <fcntl.h>
#include <termio.h>
#include <stdio.h>
static struct termio oldmode, newmode;
static int raw = 0;
extern FILE *debug;
raw_tty()
{
    if (raw)
        return(0);
    close(0);
    if (open("/dev/tty", O_RDWR | O_NDELAY) == -1)
    {   printf("Cannot open tty\n");
        fprintf(debug,"can't open tty\n");
        exit(1);
    }
    if (ioctl(0, TCGETA, &oldmode) == -1)
    {   printf("Cannot access tty settings\n");
        fprintf(debug,"ioctl error in raw_tty - can't get settings\n");
        exit(1);
    }
    ioctl(0, TCGETA, &newmode);
    newmode.c_lflag &= ~(ICANON | ECHO);
    newmode.c_cc[VMIN] = 1;
    newmode.c_cc[VTIME] = 0;
    if (ioctl(0, TCSETA, &newmode) == -1)
    {
        printf("Cannot change tty settings\n");
        fprintf(debug,"ioctl error in raw_tty - can't change settings\n");
        exit(1);
    }
    raw = 1;
}


/*---------- Function: cooked_tty ----------*/
cooked_tty()
{
    if (!raw)
        return(0);
    if (ioctl(0, TCSETA, &oldmode) == -1)
    {   printf("Cannot restore tty settings\n");
        fprintf(debug,"ioctl error in cooked_tty - can't restore settings\n");
        exit(1);
    }
    raw = 0;
}
