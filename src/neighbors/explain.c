/* %W% %G% */
static char *explanation[] =
{
"This program creates a new cell file from an existing cell file.",
"Each new cell value is derived from the cells within a neighborhood",
"of the original cell.",
"",
"You will be asked for the name of an existing cell file and the name of",
"a new cell file to hold the results. The new cell file will be created",
"in the current mapset.",
"",
"You will then be asked to specify the size of the neighborhood, which",
"is an N x N square surrounding each cell, and to select the method",
"to compute the new cell values.",
0};

explain()
{
    char **s;
    char buf[100];

    new_screen();

    for (s = explanation; *s; s++)
	printf("%s\n",*s);
    printf("\nHit RETURN to continue -->");
    G_gets(buf);

    new_screen();
}
