/*****************************************
 * I_tape_advance(fd,n)
 *
 * fd: tape file descriptor
 * n: -1 advance file, >0 n advance records
 *
 * two versions, one for systems which support
 * mtio ioctl calls [see unix manual entry mt(4)]
 * and one (slow) for those which do not
 *******************************************/

#include <grass/config.h>

#include <unistd.h>

#ifdef HAVE_SYS_MTIO_H

#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <grass/imagery.h>


int I_tape_advance(int fd,int n)
{
    struct mtop command;

    if (n < 0)
    {
	command.mt_op = MTFSF;	/* advance file */
	command.mt_count = 1;
    }
    else if (n > 0)
    {
	command.mt_op = MTFSR;	/* advance record(s) */
	command.mt_count = n;
    }
    else return 0;

    ioctl (fd, MTIOCTOP, &command);

	return 0;
}

#else

int I_tape_advance(int fd,int n)
{
    char buf[32767]; /* hope this is big enough for most records */
    if (n < 0)
	while (read (fd, buf, sizeof buf) > 0)
		;
    else
	while (n-- > 0 && read (fd, buf, sizeof buf) > 0)
		;

	return 0;
}

#endif /* HAVE_SYS_MTIO_H */
