#include "gis.h"
#include "parms.h"

header (panel, npanels, date,scale)
    char *date;
	double scale;
{
    char buf[1024];
    char *header_file;
    char *getenv();
    FILE *fd;

    static int first = 1;

    if (npanels > 1)
    {
	sprintf (buf, "%*spanel %d (of %d)",
		Pnchars()-20, "", panel, npanels);
	Ptext(buf);
	Ptext("");
    }
    if (!first) return;
    first = 0;
    if (header_file = getenv ("PAINT_HEADER"))
    {
	if (*header_file != '/')
	{
	    output (header_file, panel, npanels, date);
	    return;
	}
    }
    else
	sprintf (header_file = buf, "%s/etc/paint/header", G_gisbase());
    if (fd = fopen (header_file, "r"))
    {
	while (G_getl (buf, sizeof buf, fd))
	    output (buf, panel, npanels, date);
	sprintf(buf,"SCALE  1:%.0lf",scale);
	Ptext(buf);
	fclose (fd);
    }
}
static
output (line, panel, npanels, date)
    char *date;
    char *line;
{
    char text[1024];
    char fmt[30];
    char *get_format();
    char *buf;

    buf = line;
    *text = 0;
    while (*buf)
    {
	if (*buf == '%')
	{
	    buf++;
	    if(*buf == '%')
		strcat (text, "%");
	    else if (*buf == 'n')
	    {
		Ptext(text);
		*text = 0;
	    }
	    else if (*buf == '_')
	    {
		int i;
		for (i = Pnchars(); i>buf-line; i--)
		    strcat (text,"_");
	    }
	    else
	    {
		buf = get_format (buf, fmt);
		append ('s', fmt);
		switch (*buf)
		{
		case 'd': apply (date,fmt,text); break;
		case 'l': apply (G_location(),fmt,text); break;
		case 'L': apply (G_myname(),fmt,text); break;
		case 'c': 
			if (parms.cellfd >= 0)
			{
			    char name[100];
			    sprintf (name, "<%s> in mapset <%s>",
				parms.cellname, parms.cellmapset);
			    apply (name, fmt, text);
			}
			else
			    apply ("none", fmt, text);
			break;
		case 'm': apply (G_mapset(),fmt,text); break;
		case 'u': apply (G_whoami(),fmt,text); break;
		case 'x': apply (G_mask_info(),fmt,text); break;
		case 0: continue;
		}
	    }
	}
	else
	    append (*buf, text);
	buf++;
    }
    Ptext(text);
}
static
char *
get_format (buf, fmt)
    char *buf;
    char *fmt;
{
    strcpy (fmt, "%");

    if (*buf == '-')
    {
	append (*buf++, fmt);
	if (*buf < '1' || *buf > '9')
		return buf-1;
    }
    while (*buf >= '0' && *buf <= '9')
	append (*buf++, fmt);
    return buf;
}
static
append (c, buf)
    char c;
    char *buf;
{
    while (*buf)
	buf++;
    *buf++ = c;
    *buf = 0;
}
static
apply (buf, fmt, text)
    char *buf;
    char *fmt;
    char *text;
{
    char temp[300];

    sprintf (temp, fmt, buf);
    strcat (text, temp);
}
