#include "include.h"
#include "yes_no.h"

list_analysis(print)
    int print ;
{
    char command[1024];
    struct Histogram *histo;
    int ncats;
    static char *tempfile = NULL;
    int count;
    int i ;
    int map ;
    long cur_wgt ;
    long cur_cat ;
    long next_wgt ;
    long next_cat ;
    int last_cat ;
    int len;
    FILE *fopen(), *fd ;

    if (tempfile == NULL)
	tempfile = G_tempfile();
    fd = fopen (tempfile, "w");
    if (fd == NULL)
    {
	G_warning ("No tempfile available");
	return -1;
    }

    fprintf(fd,"\n\n") ;

    for (map=0; map<MAX_MAPS; map++)
    {
	if (mapdef[map].used == NO)
	    continue;

	fprintf(fd,"map: %s\n", mapdef[map].name) ;
	fprintf (fd, "  weight: categories");
	histo = &mapdef[map].histo;
	ncats = G_get_histogram_num (histo);
	G_sort_histogram_by_count (histo);

	cur_wgt = G_get_histogram_count (0, histo)-1;

	count = 0;
	len = 0;
	for (i=0; i < ncats ; i++) 
	{
	    next_wgt = G_get_histogram_count (i, histo);
	    next_cat = G_get_histogram_cat (i, histo);
	    if (cur_wgt != next_wgt)
	    {
		print_range (fd, cur_cat, count, len, 9);
		fprintf (fd, "\n%8ld:", cur_wgt = next_wgt);
		cur_cat = next_cat;
		count = 0;
		len = 9;
	    }
	    else if (next_cat != cur_cat+count)
	    {
		len = print_range (fd, cur_cat, count, len, 9);
		cur_cat = next_cat;
		count = 0;
	    }
	    count++;
	}
	print_range (fd, cur_cat, count, len, 9);
	fprintf (fd, "\n");
	G_sort_histogram (histo);
    }

    fprintf(fd,"\nAnalysis to be done by ") ;
    switch(analysis_type)
    {
    case ADD:
	    fprintf(fd,"ADDING") ;
	    break ;
    case MULT:
	    fprintf(fd,"MULTIPLYING") ;
	    break ;
    default:
	    fprintf(fd,"UNKNOWN") ;
	    break ;
    }
    fprintf(fd," assigned weights.\n\n") ;

    fclose (fd);
    sprintf (command, "%s %s", print?"lpr":"more",tempfile);
    system (command) ;
    unlink (tempfile);
}

print_range (fd, cur, count, len, indent)
    FILE *fd;
    long cur;
{
    int n;
    char buf[40];

    if(count-- <= 0)
	return 0;
    if (count)
	sprintf (buf, " %ld%s%ld", cur, cur < 0 ? " thru " : "-", cur+count);
    else
	sprintf (buf, " %ld", cur);
    n = strlen (buf);

    if (len+n > 70)
    {
	fprintf (fd, "\n");
	for (len = 0; len < indent; len++)
	    fprintf (fd, " ");
    }
    fprintf (fd, "%s", buf);
    return len+n;
}
