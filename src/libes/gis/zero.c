/****************************************************************
 *  G_zero (buf, i)
 *     char *buf           buffer to be zeroed
 *     int i               number of bytes to be zeroed
 *
 *  Zeros out a buffer to 'i' bytes
 ****************************************************************/

G_zero (buf, i)
    register char *buf ;
    register int i ;
{
    while(i--)
	*buf++ = 0 ;
}
