int center (char *msg, int len)
{
	int lead;

	lead = (len - strlen(msg)) / 2;

	while (lead-- > 0)
		fprintf (stdout," ");
	fprintf (stdout,"%s\n", msg);

	return 0;
}
