yes(question)
    char *question;
{
    char answer [100];

    printf("%s? ", question);
    while (1)
    {
	printf("(y/n) ");
	input (answer);
	switch (*answer)
	{
	    case 'n': case 'N': return (0);
	    case 'y': case 'Y': return (1);
	}
    }
}
