int 
main (void)
{
	char buf[200];
	int map[1000];
	int nmap;
	long offset;
	long *index;
	int nchars;
	int n;
	int size;

	int font;

	while (1)
	{
		fprintf (stdout,"select font (ls for list): ");
		if (!gets(buf)) exit(0);
		if (*buf == 0) exit(0);
		if (strcmp (buf, "ls") == 0)
		{
			system ("cd fonts; ls *.hmp | sed 's/.hmp//' | fmt");
			continue ;
		}
		if (-1 == init_font(buf))
			exit(-1) ;

		nmap = 0176 - 040 ;
		while (1)
		{
			fprintf (stdout,"enter character: ");
			if (!gets(buf)) exit(0);
			if (*buf == 0) break;
			showchar (*buf);
		}
	}
}
