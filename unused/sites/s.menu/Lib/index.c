char *index (char *string, int c)
{
	while (*string)
		if (*string == c)
			return string;
		else
			string++;
	
	return ((char *) 0);
}
