G_newlines_to_spaces(s)
    char *s;
{
    while (*s)
    {
	if (*s == '\n') *s = ' ';
	s++;
    }
}
