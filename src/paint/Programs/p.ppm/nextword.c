nextword(word)
    char *word;
{
    while (1)
    {
	if (scanf("%s",word) != 1) return 0;
	if (*word != '#') return 1;
	if (!eol()) return 0;
    }
}

eol()
{
    int c;
    while ((c = getchar()) != '\n') 
		;
    return c > 0;
}
