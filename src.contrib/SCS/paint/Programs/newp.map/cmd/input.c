#include <stdio.h>

extern FILE *tracefd;
extern FILE *inputfd ;

input(level, buf, help)
    char *buf;
    char *help[];
{
    char temp1[10], temp2[3];
    int i;

    do
    {
	if (level && isatty(fileno(inputfd)))
	/*
	    printf ("%s ",level==1?">":">>>");
		*/
	if (level == 1) printf ("> ");
	if (level == 2) printf (">>> ");
	if (level == 3) printf (">>>>> ");

	if (!G_getl(buf,1024,inputfd))
	{
	    if (inputfd != stdin)
	    {
		fclose (inputfd);
		inputfd = stdin;
	    }
	    return 0;
	}

	if (tracefd != NULL)
	{
	    fprintf (tracefd,"%s\n",buf);
	    fflush (tracefd);
	}

	if (sscanf (buf, "%5s %1s", temp1, temp2) == 1)
	{
	    if(strcmp (temp1, "end") == 0)
		return 0;
	    if(strcmp (temp1, "exit") == 0)
		exit(0);
	    if (strcmp (temp1, "help") == 0)
	    {
		*buf = '#' ;
		if (help != NULL)
		{
		    for (i = 0; help[i] && help[i][0]; i++)
			printf ("%s\n", help[i]);
		    printf ("enter 'end' when done, 'exit' to quit\n");
		}
	    }
	}
    }
    while (*buf == '#');

    if (level)
	add_to_session (level > 1, buf);

    if (*buf == '\\')
	for (i = 0; buf[i] = buf[i+1]; i++)
		;
    return 1;
}

gobble_input()
{
    char buf[1024];

    if (inputfd != stdin)
    {
	fclose (inputfd);
	inputfd = stdin;
    }
    else if (!isatty(0))
	while (input(0,buf,(char **)NULL))
		;
}
