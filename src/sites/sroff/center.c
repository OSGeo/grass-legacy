center (buf, width)

	char *buf;
{
	char *xalloc();

	char *temp;
	char *t;
	int lead;

	lead = (width - strlen(buf))/2;
	if (lead <= 0)
		return;
	
	temp = xalloc (width+1);
	for (t = temp; lead-- > 0; t++)
		*t = ' ';
	strcpy (t, buf);
	strcpy (buf, temp);
	free (temp);
}
