yes(question)
    char *question;
{
    char answer [100];

    fprintf (stdout,"%s? ", question);
    while (1)
    {
	fprintf (stdout,"(y/n) ");
	input (answer);
	switch (*answer)
	{
	    case 'n': case 'N': return (0);
	    case 'y': case 'Y': return (1);
	}
    }
}
