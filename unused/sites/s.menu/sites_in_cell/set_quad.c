static char *quad_set[] = {

"The Quad size for sites_in_cell is fixed at 0",
"",

0};

int 
set_quad (int *quadsize)
{
    char **s;
    char buf[300];
    int print_prompt;

/*
 * inform user that the value for quad size is fixed at 0
 * for the Sites_in_cell function
 */
    quadsize=0;
    new_screen();
    print_prompt = 1;
    while (1)
    {
	if (print_prompt)
	{
	    for (s = quad_set; *s; s++)
		fprintf (stdout,"%s\n", *s);
	}
	fprintf (stdout,"\nEnter <stop> to exit Sites_in_cell: ");
	print_prompt = 1;
	if (!G_gets(buf)) continue;
	print_prompt = 0;
	fprintf (stdout,"<<%s>>\n", buf);
	if (strcmp(buf,"stop") == 0)
	    return 0;
         else
            return 1;
    }

    return 1;
}
