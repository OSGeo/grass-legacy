/* Program: dvgatext		Author: Paul W. Carlson		Nov. 1988
 *
 * This program puts the Orchid Designer VGA in text mode when the system
 * is booted.
 */

#include <fcntl.h>
#define GLOBAL
#include "io.h"

main()
{
    struct io_args args;
    static char version[8];
    int n;

    if ((viofd = open("/dev/vio", 0)) == -1)
    {	perror("Open /dev/vio");
	exit(1);
    }
    ioctl(viofd, VIO_TEXTMODE, &args);
    args.ptr = (unsigned char *)version;
    ioctl(viofd, VIO_VERSION, &args);
    close(viofd);

    printf("\n\n\311");
    for (n = 0; n < 50; n++) printf("\315");
    printf("\273\n");
    printf("\272  ");
    printf("Orchid Designer VGA Device Driver Version %s", version);
    printf("  \272\n");
    printf("\272  ");
    printf("   Author: Paul W. Carlson     FTS 832-6795   ");
    printf("  \272\n");
    printf("\310");
    for (n = 0; n < 50; n++) printf("\315");
    printf("\274\n\n");
}
