static char *help[]=
{
    "enter pattern using blanks (or 0) and digits (1-9) to define pattern",
    "  or",
    "color # color (to preset a pattern color)",
    ""
};

input_pattern (name)
    char *name;
{
    char buf[1024];

    begin_pattern(name);

    while (input(-2,buf,help))
	if(store_pattern(buf) < 0)
	{
	    error (buf,"","illegal color request");
	}

    end_pattern ();
}
