static char *explanation[] =
{
"This program creates a new raster file from an existing raster file.",
"Each new cell value is derived from neighboring cells.",
"",
"You will be asked for the name of an existing raster file and the name of",
"a new raster file to hold the results.",
"",
"You will then be asked to specify the size of the neighborhood which",
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
