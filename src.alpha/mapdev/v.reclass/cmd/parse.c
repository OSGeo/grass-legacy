static char rcsid[]="$Header: /usr3/4.0/src.contrib/SCS/vector/v.reclass/cmd/RCS/parse.c,v 1.1 1992/09/28 06:06:54 grass Exp grass $";
#include "rule.h"

static char *cur;
static int state;

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

parse (line, rules, tail, cats)
    char *line;
    RULE **rules, **tail;
    struct Categories *cats;
{
    char *label;
    char *save;
    CELL v;
    CELL lo[100], hi[100], new;
    int count;
    int i;

    cur = line;
    state = 0;
    count = 0;
    label = "";

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
	    lo[count] = hi[count] = v;
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
	    if(lo[count-1] > v)
	    {
		hi[count-1] = lo[count-1];
		lo[count-1] = v;
	    }
	    else
		hi[count-1] = v;
	
	    state = 1;
	    continue;
	case 4:
	    if (!scan_value (&v))
		return -1;
	    new = v;
	    state = 5;
	    continue;
	case 5:
	    label = cur;
	    cur = "";	/* force break from while */
	}
    }
    if (state > 0 && state < 5)
	return -1;
    
    for (i = 0; i < count; i++)
    {
	add_rule (tail, lo[i], hi[i], new);
	if (*rules == NULL)
	    *rules = *tail;
	if (*label)
	    G_set_cat (new, label, cats);
    }
    return count;
}
