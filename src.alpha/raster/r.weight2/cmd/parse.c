#include "gis.h"
static char *cur;
static int state;

parse (line)
    char *line;
{
    char *save;
    CELL v;
    int count;

    cur = line;
    state = 0;
    count = 0;

    while (*cur == ' ' || *cur == '\t' || *cur == '\n')
	cur++;
    while (*cur)
    {
	while (*cur == ' ' || *cur == '\t' || *cur == '\n')
	    cur++;
 
	switch (state)
	{
	case 0:
	    save = cur ;
	    if (!scan_value(&v))
		return -1;
	    state = 1;
	    cur = save;
	    continue;
	case 1:
	    if (*cur == '=')
	    {
		cur++;
		state = 4;
		continue;
	    }
	    if (!scan_value (&v))
		return -1;
	    count++;
	    state = 2;
	    continue;
	case 2:
	    state = 1;
	    if (strncmp (cur, "thru", 4) != 0)
		continue;
	    cur += 4;
	    if (*cur != ' ' && *cur != '\t')
		return -1;
	    state = 3;
	    continue;
	case 3:
	    if (!scan_value (&v))
		return -1;
	    state = 1;
	    continue;
	case 4:
	    if (!scan_value (&v))
		return -1;
	    state = 5;
	    continue;
	case 5:
	    cur = "";	/* force break from while */
	}
    }
    if (state > 0 && state < 5)
	return -1;
    
    return count;
}

static
scan_value (v)
    CELL *v;
{
    int sign;

    sign = 1;
    if (*cur == '-')
    {
	sign = -1;
	cur++;
    }
    if (*cur < '0' || *cur > '9')
	return 0;

    *v = *cur++ - '0' ;
    while (*cur >= '0'  && *cur <= '9')
	*v = *v * 10 + *cur++ - '0';
    *v *= sign;
    switch (*cur)
    {
    case 0:
    case ' ':
    case '\t':
    case '\n':
    case '=':
		return 1;
    default:
		return 0;
    }
}
