/* copy blocks of memory - eg. structures */
memcopy (des, src, n)
	char *des;
	char *src;
	int n;
{
	while (n-- > 0)
		*des++ = *src++;
}
