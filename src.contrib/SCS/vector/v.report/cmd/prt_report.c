/*
 * $Id$
 */

#include "global.h"
#include <string.h>

int 
print_report (int unit1, int unit2)
{
    int ns,nl,nx;
    char num[30];
    int len, new;
    long *cats, *prev;
    int first;
    int i;
    int divider_level;
    int after_header;
    int need_format;
    int with_stats;
    char *cp;
    int spacing;
    char dot;

/* examine units, determine output format */
    for (i = unit1; i <= unit2; i++)
    {
	need_format = 1;
	unit[i].label[0] = "";
	unit[i].label[1] = "";

	switch (unit[i].type)
	{
	case COUNTS:
	    need_format = 0;
	    unit[i].len = 5;
	    unit[i].label[0] = "";
	    unit[i].label[1] = "count";
	    ns = 0;
	    sprintf (num, "%ld", count_sum(&ns,-1));
	    len = strlen (num);
	    if (len > unit[i].len)
		unit[i].len = len;
	    break;

	case SQ_METERS:
	    unit[i].label[0] = "square";
	    unit[i].label[1] = "meters";
	    unit[i].factor   = 1.0;
	    break;

	case LN_METERS:
	    unit[i].label[0] = "";
	    unit[i].label[1] = "meters";
	    unit[i].factor   = 1.0;
	    break;

	case SQ_KILOMETERS:
	    unit[i].label[0] = "  square  ";
	    unit[i].label[1] = "kilometers";
	    unit[i].factor   = 1.0e-6;
	    break;

	case LN_KILOMETERS:
	    unit[i].label[0] = "";
	    unit[i].label[1] = "kilometers";
	    unit[i].factor   = 1.0e-3;
	    break;

	case ACRES:
	    unit[i].label[0] = "";
	    unit[i].label[1] = "acres";
	    unit[i].factor   = 2.471e-4;
	    break;

	case HECTARES:
	    unit[i].label[0] = "";
	    unit[i].label[1] = "hectares";
	    unit[i].factor   = 1.0e-4;
	    break;

	case SQ_MILES:
	    unit[i].label[0] = "square";
	    unit[i].label[1] = " miles";
	    unit[i].factor   = 3.861e-7;
	    break;

	case LN_MILES:
	    unit[i].label[0] = "";
	    unit[i].label[1] = " miles";
	    unit[i].factor   = 6.213e-4;
	    break;

	case LN_FEET:
	    unit[i].label[0] = "";
	    unit[i].label[1] = " feet";
	    unit[i].factor   = 3.2808;
	    break;

	case SQ_FEET:
	    unit[i].label[0] = "square";
	    unit[i].label[1] = "  feet";
	    unit[i].factor   = 10.7639;
	    break;

	default:
	    fprintf (stdout,"Unit %d not yet supported\n", unit[i].type);
	    exit(1);
	}
	if (need_format)
	{
	    unit[i].dp = 10;
	    unit[i].len = 14;
	    unit[i].eformat = 0;
	    ns = 0;
            if (unit[i].type == LN_METERS ||
                unit[i].type == LN_FEET ||
                unit[i].type == LN_KILOMETERS ||
                unit[i].type == LN_MILES ) 
	        format_parms (len_sum(&ns,-1)*unit[i].factor,
		    unit[i].len, &unit[i].eformat, &unit[i].dp);
            else
	        format_parms (area_sum(&ns,-1)*unit[i].factor,
		    unit[i].len, &unit[i].eformat, &unit[i].dp);
	}
    }

/* figure out how big the category numbers are when printed */
    for (nl = 0; nl < nlayers; nl++)
	layers[nl].nlen = 0;

    for (ns = 0; ns < nstats; ns++)
    {
	cats = Gstats[ns].cats;
	for (nl = 0; nl < nlayers; nl++)
	{
	    sprintf(num, "%ld", cats[nl]);
	    len = strlen (num);
	    if (len > layers[nl].nlen)
		layers[nl].nlen = len;
	}
    }

/* compute maximum category description lengths */
    len = page_width-2;
    for (i = unit1; i <= unit2; i++)
	len -= (unit[i].len + 1);
    for (nl = 0; nl < nlayers; nl++)
    {
	len -= (layers[nl].nlen + 1);
	layers[nl].clen = len;
    }

/* print the report */

    header(unit1, unit2);
    after_header = 1;
    new = 1;

    divider_level = -1;
    for (ns = 0; ns < nstats; ns++)
    {
	cats  = Gstats[ns].cats;

/* determine the number of lines needed to print the cat labels 
 * by pretending to print the labels and counting the number of
 * print calls needed
 */

	if (page_length > 0)
	{
	    i = 0;
	    for (nl = 0; nl < nlayers; nl++)
	    {
		cp = G_get_cat ((CELL) cats[nl], &layers[nl].labels);
		while (cp)
		{
		    i++;
		    cp = print_label (cp, layers[nl].clen, 0, 0, ' ');
		}
	    }
	    if (nunits) i+=nlayers; /* divider lines */

    /* if we don't have enough lines, go to a new page */
	    if (nlines <= i+2)
	    {
		trailer();
		header(unit1, unit2);
		after_header = 1;
		new = 2;
	    }
	}

/* print the report */
	for (nl = 0; nl < nlayers; nl++)
	{
	    if (new || (prev[nl] != cats[nl]))
	    {
	    /* divider line between layers */

		if (nunits && divider_level != nl && !after_header)
		{
		    for (nx = 0 ; nx < nl ; nx++)
			fprintf (stdout,"|%*s", layers[nx].nlen, "");
		    fprintf (stdout,"|");
		    for (nx = layers[nl].clen+layers[nx].nlen+1; nx > 0; nx--)
			fprintf (stdout,"-");
		    for (i = unit1; i <= unit2; i++)
		    {
			fprintf (stdout,"|");
			for (nx = unit[i].len; nx > 0; nx--)
			    fprintf (stdout,"-");
		    }
		    fprintf (stdout,"|");
		    newline();
		}
		divider_level = nl;
		after_header = 0;

		first = 1;
		if (!new)
		    new = 1 ;

		cp = G_get_cat ((CELL) cats[nl], &layers[nl].labels);
		while(cp)
		{
		    for (nx = 0 ; nx < nl ; nx++)
			fprintf (stdout,"|%*s", layers[nx].nlen, "");
		    if (first)
			fprintf (stdout,"|%*ld|", layers[nl].nlen, cats[nl]);
		    else
			fprintf (stdout,"|%*s|", layers[nl].nlen, "");

		    with_stats = nunits && first;
		    if (new == 2 && nl != nlayers-1)
			with_stats = 0;
		    if (with_stats)
		    {
			if (nl != nlayers-1)
			{
			    spacing = 0;
			    dot = '_';
			}
			else
			{
			    spacing = 2;
			    dot = '.';
			}
		    }
		    else
		    {
			spacing = 0;
			dot = ' ';
		    }
		    cp = print_label (cp, layers[nl].clen, 1, spacing, dot);
		    if (with_stats)
		    {
			for (i = unit1; i <= unit2; i++)
			    print_unit(i,ns,nl);
		    }
		    else
		    {
			for (i = unit1; i <= unit2; i++)
			    fprintf (stdout,"|%*s", unit[i].len, "");
		    }
		    fprintf (stdout,"|");
		    newline();
		    first = 0;
		}
	    }
	}
	new = 0;
	prev = cats;
    }
    /* overall totals */
    if (nunits)
    {
	divider("|");
	print_label ("|TOTAL", layers[0].nlen+layers[0].clen+2, 1, 0, ' ');
	for (i = unit1; i <= unit2; i++)
	    print_unit(i,0,-1);
	fprintf (stdout,"|");
	newline();
    }
    trailer();

    return 0;
}
