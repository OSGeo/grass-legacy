/* parse_mon - parse monitorcap entry */

#include <stdio.h>
#include "monitors.h"

static FILE *monitors = NULL;
static struct MON_CAP cap;

struct MON_CAP *
R_parse_monitorcap(file,field,key)
    int field;
    char *key, *file;
{
    int match, rewound;
    char line[1024];
    FILE *fopen();
    char *p, *malloc(), *substr();
    extern FILE *monitors;
    extern struct MON_CAP cap;

    rewound = 0;
    if (!(field == MON_NEXT || field == MON_NAME ||
	  field == MON_PATH || field == MON_LINK || field == MON_CLOSE))
	    return(NULL);
    if (monitors == NULL)
    {
	if ((monitors = fopen(file,"r")) == NULL)
	    return(NULL);
    }
    else
    {
	if (field == MON_CLOSE)
	{
	    fclose(monitors);
	    monitors = NULL;
	    return(NULL);
	}
    }
    while (-1)
    {
	if (read_line(monitors,line,sizeof line))
	{
	    if (field == MON_NEXT)
		return(NULL);
	    rewind(monitors);
	    if (read_line(monitors,line,sizeof line) || rewound)
		return(NULL);
	    rewound = -1;
	}
	cap.path = cap.comment = cap.link = cap.tty = cap.where = NULL;
	if ((cap.name = malloc(strlen(line)+1)) == NULL)
		return(NULL);
	strcpy(cap.name,line);
	if ((p = substr(":",cap.name)) != NULL)
	{
	    *p++ = '\0';
	    cap.path = p;
	    if ((p = substr(":",p)) != NULL)
	    {
		*p++ = '\0';
		cap.comment = p;
		if ((p = substr(":",p)) != NULL)
		{
		    *p++ = '\0';
		    cap.link = p;
		    if ((p = substr(":",p)) != NULL)
		    {
			*p++ = '\0';
			cap.tty = p;
			if ((p = substr(":",p)) != NULL)
			{
			    *p++ = 0;
			    cap.where = p;
			    if ((p = substr("\n",p)) != NULL)
				*p = '\0';
			}
		    }
		}
	    }
	}
	if (field == MON_NEXT || (field == MON_NAME && !strcmp(key,cap.name))
	|| (field == MON_PATH && !strcmp(key, cap.path))
	|| (field == MON_LINK && !strcmp(key,cap.link)))
		return(&cap);
	else
		free(cap.name);
    }
}

/* read_line - read a line, possibly continued with a "\" into a buffer */

static
read_line(file,line,size)
    FILE *file;				/* file from which to read */
    char *line;				/* buffer in which to put it */
    int size;				/* size of buffer */
{
    int length, full_line, eof, done;
    char c, last_c;

    *line ='\0';
    for (length = full_line = eof = 0; !full_line && !eof; )
    {					/*   one entire line at a time */
	eof = (fgets(line + length,size - length - 1,file) == NULL);
	length = strlen(line) - 1;
	if (*(line + length) == '\n')
	{
	    if (*(line + length - 1) == '\\')
		    --length;
	    else
		    full_line = -1;
	}
	else
	{
	    if (length != -1)
	    {
		fprintf(stderr,"error:  input line too long\n");
		full_line = -1;
		last_c = c = ' ';
		for (done = 0; !done; )
		{
		    if((c = getc(file)) != EOF)
		    {
			if (c == '\n' && last_c != '\\')
			    done = -1;
			else
			    last_c = c;
		    }
		    else
			eof = done = -1;
		}
	    }
	}
    }
    return(eof);
}

/* substr - find substring in a string.  returns pointer to start of */
/* substring or NULL if not found */

static char *
substr(string,buffer)
    char *string, *buffer;
{
    int start, i, found;
    char c, d;

    start = i = found = 0;
    while ((c = *(buffer + start + i)) != '\0' && !found)
    {
	if (c == *(string + i))
	{
	    if (*(string + ++i) == '\0')
		found = -1;
	}
	else
	{
	    start++;
	    i = 0;
	}
    }
    if (found)
	return(buffer + start);
    else
	return(NULL);
}
