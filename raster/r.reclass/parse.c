#include "rule.h"
#include <string.h>

static int scan_value (CELL *);
static char *cur;
static int state;
int default_rule=0;
int default_to_itself=0;
char *default_label;
CELL DEFAULT;

int parse (char *line,RULE **rules,RULE **tail,struct Categories *cats)
{
    char *label;
    char *save;
    CELL v;
    CELL lo[100], hi[100], new=(CELL)0;
    int count;
    int i, last_null=0;

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
            save = cur;
  	    if(*cur == '*') /* default rule */
            {
              default_rule = 1;
              state = 1;
              cur++;
              continue;
            }
	    if (!scan_value(&v))
		return -1;
            if(G_is_c_null_value(&v)) 
            {
              G_warning("can't have null on the left-hand side of the rule");;
              return -1;
            }
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
	    if(default_rule) 
	        return -1;
	    if (!scan_value (&v))
		return -1;
            if(G_is_c_null_value(&v)) last_null = 1;
            else last_null = 0;
	    lo[count] = hi[count] = v;
	    count++;
	    state = 2;
	    continue;
	case 2:
	    state = 1;
	    if (strncmp (cur, "thru", 4) != 0)
		continue;
            if(last_null)
            {
               G_warning("can't have null on the left-hand side of the rule");;
               return -1;
            }
	    cur += 4;
	    if (*cur != ' ' && *cur != '\t')
		return -1;
	    state = 3;
	    continue;
	case 3:
	    if (!scan_value (&v))
		return -1;
            if(G_is_c_null_value(&v)) 
            {
               G_warning("can't have null on the left-hand side of the rule");;
               return -1;
            }

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
            if(*cur == '*' && default_rule)
            {
               cur++;
               new = 0;
               default_to_itself = 1;
               state = 5;
               continue;
            }

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
    if(default_rule) 
    {
      DEFAULT = new;
      default_label = G_store((*label ? label : ""));
      return 1;
    }

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

static int scan_value (CELL *v)
{
    int sign;

    if(strncmp(cur, "null", 4) == 0 || strncmp(cur, "NULL", 4) == 0)
    {
       cur += 4;
       G_set_c_null_value(v, 1);
    }
    else
    {
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
    }
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
