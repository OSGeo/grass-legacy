char *
index (string, c)

	char *string;
	char c;
{
	while (*string)
		if (*string == c)
			return string;
		else
			string++;
	
	return ((char *) 0);
}
