/* %W% %G% */

ask(old_min, old_max, new_min, new_max, title)
    long *old_min;
    long *old_max;
    long *new_min;
    long *new_max;
    char *title;
{
    char err1[50], err2[50];

    *err1 = *err2 = 0;

    V_clear();

    V_line (0, "Enter the data range to be rescaled");
    V_line (1, "(data outside the range you specify will become 0)");
    V_line (2,"  min:");
    V_line (3,"  max:");

    V_line (5, "Enter the resultant data range");
    V_line (6,"  min:");
    V_line (7,"  max:");

    V_line (10,"The following is a title for the resultant map");

    V_ques (old_min, 'l', 2, 8, 8);
    V_ques (old_max, 'l', 3, 8, 8);
    V_ques (new_min, 'l', 6, 8, 8);
    V_ques (new_max, 'l', 7, 8, 8);

    V_const (err1, 's', 3, 30, 30);
    V_const (err2, 's', 7, 30, 30);

    V_ques (title, 's', 11, 1, 78);

    do
    {
	G_strip (title);
	V_intrpt_ok();
	if(!V_call())
	    exit(0);
	if (*old_min > *old_max)
	    sprintf(err1, "** min can't be larger than max");
	else
	    *err1 = 0;
	if (*new_min > *new_max)
	    sprintf(err2, "** min can't be larger than max");
	else
	    *err2 = 0;
    }
    while (*err1 || *err2);
    G_strip (title);
}
