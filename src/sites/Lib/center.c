center (msg,len)	char *msg;
{
	int lead;

	lead = (len - strlen(msg)) / 2;

	while (lead-- > 0)
		printf(" ");
	printf("%s\n", msg);
}
