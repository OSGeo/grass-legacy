/******************************************************

NAME:		ask()

FUNCTION:	print a prompt, input user response

USAGE:		ask (prompt, answer)

ACTION:		ctrl-d will cause exit
		leading white space is removed
		null entry causes ask to try again

PARMS:		prompt: prompt to print
		answer: buffer to read answer into
			if NULL just wait for c/r
******************************************************/
#include <stdio.h>
ask (prompt, answer, dflt)
    char *prompt;
    char *answer;
    char *dflt;
{
    char *a;
    char *b;
    char buf[200];
    int nullok;


    if (nullok = (answer == 0))
	answer = buf;
    if (dflt != NULL)
	nullok = 1;
    do
    {
	printf ("%s", prompt);
	if (dflt)
	    printf (" [%s] ", dflt);
	if (!gets(answer))
	{
	    printf("\n");
	    exit(1);
	}
	for (a = b = answer; *b == ' ' || *b == '\t'; b++)
		;
	while (*a++ = *b++)
		;

	if (strcmp (answer,"exit") == 0)
	    exit(1);

    } while (!nullok && *answer == 0);
    if (*answer == 0 && dflt)
	strcpy (answer, dflt);
}
