#include <signal.h>
#include <setjmp.h>
#include <termio.h>

jmp_buf x;

main()
{
    printf ("begin\n");
    g();
    printf ("end\n");
}

sigalarm(n)
{
    longjmp (x,1);
}
g()
{
    char buf[1024];
    int fd;

    fd = open ("/dev/tty01", 2, 0);
    set(fd);
    if (setjmp(x))
    {
	printf ("timeout\n");
	return 0;
    }
    signal (SIGALRM, sigalarm);
    alarm(3);
    write (fd, "M5\r", 3);
    while(read(fd,buf,1) <= 0);
    alarm(0);
    return 1;
}
set(fd)
{
    struct termio termio;

    if (ioctl (fd, TCGETA, &termio) < 0)
	perror ("TCGETA");

    termio.c_iflag = IGNBRK | IGNPAR ;
    termio.c_oflag = 0 ;
    termio.c_cflag = B9600 | CS8 | CREAD| HUPCL | CLOCAL ;
    termio.c_lflag = 0 ;

    termio.c_cc[VEOF] = 0 ;
    termio.c_cc[VEOL] = 0 ;
    if (ioctl (fd, TCSETA, &termio) < 0)
	perror ("TCSETA");
}
