/* EGA program: egaversion	Paul W. Carlson		Jan. 1989
 */

#define GLOBAL
#include "lib/ega_io.h"

main()
{
    struct io_args args;
    static char version[8];
    int n;

    if ((egafd = open("/dev/ega", 0)) == -1)
    {	perror("Open /dev/ega");
	exit(1);
    }
    args.ptr1 = (unsigned char *)version;
    ioctl(egafd, EGA_VERSION, &args);
    close(egafd);

}
