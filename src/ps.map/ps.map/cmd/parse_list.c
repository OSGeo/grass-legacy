/***********************************************************
 * parse_number_list (buf, list)
 *   char *buf; int **list;
 *
 * buf is a comma separated list of non-negative numbers
 * or number ranges: 1,2,6-10,12
 *
 * actual usage is
 *   char buf[300]; int *list;
 *
 *   count = parse_number_list (buf, &list);
 *
 *   for (i = 0; i < count; i += 2)
 *	{
 *          min = list[i];
 *	    max = list[i+1];
 *	}
 *
 * count will be negative if list is not valid
 ********************************************************/
parse_number_list (buf, list)
    char *buf;
    int **list;
{
    int count;
    int a,b;
    int *lp;
    char *malloc();
    char *realloc();

    count = 0;
    lp = (int *) malloc(sizeof(int));
    while (*buf)
    {
	while (*buf == ' ' || *buf == '\t' || *buf == '\n' || *buf == ',')
	    buf++;
	if (sscanf (buf, "%d-%d", &a, &b) == 2)
	{
	    if (a > b)
	    {
		int t;
		t = a;
		a = b;
		b = t;
	    }

	    lp = (int *) realloc (lp, (count+2) * sizeof(int));
	    lp[count++] = a;
	    lp[count++] = b;
	}
	else if (sscanf (buf, "%d", &a) == 1)
	{
	    lp = (int *) realloc (lp, (count+2) * sizeof(int));
	    lp[count++] = a;
	    lp[count++] = a;
	}
	else
	{
	    free (lp);
	    return -1;
	}
	while (*buf && (*buf != ','))
	    buf++;
    }
    *list = lp;
    return count;
}
