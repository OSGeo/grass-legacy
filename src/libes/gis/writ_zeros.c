/*******************************************************
 * 
 * G_write_zeros (fd ,n)
 *   int fd
 *   long n
 *
 * writes n bytes of zero to file descriptor fd
 ******************************************************/

G_write_zeros(fd,n)
    long n;
{
    char zeros[1024];
    register char *z;
    register int i;

/*
 * there is a subtle gotcha to be avoided here
 * i must be an int for the write, but n can be long 
 * must be careful not to cast long to int, hence must
 * avoid i = n unless n is within range of int
 */
    if (n <= 0)
	return;

/* fill zeros buffer with zeros */
    if (n > sizeof zeros)
	i = sizeof zeros;
    else
	i = n;	/* this is ok here */

    z = zeros;
    while (i--)
	*z++ = 0;

/* write n zeros to fd */
    while (n > 0)
    {
	if (n > sizeof zeros)
	    i = sizeof zeros;
	else
	    i = n;	/* this is ok here */
	write (fd, zeros, i);
	n -= i;
    }
}
