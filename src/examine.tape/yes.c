/* %W% %G% */
yes()
{
    char answer[30];

    while(1)
    {
	ask ("(y/n) ", answer);

	switch (*answer)
	{
	case 'y': case 'Y': return (1);
	case 'n': case 'N': return (0);
	}
    }
}
