#include <unistd.h>
#include <grass/gis.h>


/*!
 * \fn int G_write_zeros (int fd, size_t n)
 *
 * \brief Writes <b>n</b> bytes of 9 to file descriptor <b>fd</b>
 *
 * \param fd file descriptor
 * \param n number of bytes to write
 * \return int 0 is always returned
 */

int G_write_zeros (int fd, size_t n)
{
    char zeros[1024];
    char *z;
    int i;

    if (n <= 0)
	return 0;

    /* There is a subtle gotcha to be avoided here.
     *
     * i must be an int for the write, but n (size_t) can be long or larger.
     * Must be careful not to cast long to int, hence
     * avoid i = n unless n is within range of int */

    /* fill zeros buffer with zeros */
    if (n > sizeof(zeros))
	i = sizeof(zeros);
    else
	i = n;	/* this is ok here */

    z = zeros;
    while (i--)
	*z++ = 0;

    /* write n zeros to fd */
    while (n > 0)
    {
	if (n > sizeof(zeros))
	    i = sizeof(zeros);
	else
	    i = n;	/* this is ok here */

	write (fd, zeros, i);
	n -= i;
    }

    return 0;
}
