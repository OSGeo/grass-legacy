#include <string.h>
#include "gis.h"
#include "site.h"
#include "options.h"
#include "local_proto.h"

#define MAX_SITE_RULES 12

static int Rulenum=0;
static Site_rules SR[MAX_SITE_RULES];

/* Syntax of rule string is: 

	[DCA]#.RANGE 
	where RANGE is one of #-#, ge#, le#, eq#

	examples:

	      D1.le-88.125    easting less than (or =) -88.125  
	      D3.100-200      dimension 3 in range 100 to 200
	      C.eq7.2         category equal 7.2 (note no number after C)
	      A2.ge5000       2nd double attribute greater than (or =) 5000
	      A1.-2.0--1.0    1st double attribute in range -2.0 to -1.0

    Returns -1 on error, 1 on success.

    sets a new rule each time it's called up to MAX_SITE_RULES
*/


int 
parse_set_rules (char *str)
{
Site_rules *srules;
int pos=0, len, tmpi;
double tmpd1, tmpd2;
char *r;

    if (Rulenum >= MAX_SITE_RULES) return (-1);

    srules = &(SR[Rulenum]);

    if(!str || !srules) return (-1);

    len=strlen(str);

    srules->param = srules->range_type = srules->use_this = 0;
    srules->lowval = srules->highval = srules->val = 0.0;
    switch(str[pos]){
	case 'D': 
	case 'd':
	    srules->use_this = USE_DIM;
	    if(++pos > len) return (-1);
	    if(sscanf(str+pos,"%d",&tmpi) != 1) return(-1);
	    srules->param = tmpi;
	    break;
	case 'C':
	case 'c':
	    srules->use_this = USE_CAT;
	    break;
	case 'A':  /* attribute */
	case 'a':
	    srules->use_this = USE_DBL;
	    if(++pos > len) return (-1);
	    if(sscanf(str+pos,"%d",&tmpi) != 1) return(-1);
	    srules->param = tmpi;
	    break;
	default:
	    return(-1);
    }
    /* find the first decimal */
    if(NULL==(r=strchr(str,'.'))) return(-1);
    pos=0;
    len = strlen(r);
    if(++pos > len) return (-1);
    switch(r[pos]){
	case 'l':
	case 'L':
	    srules->range_type = RANGE_LESS;
	    pos+=2;
	    if(pos > len) return (-1);
	    if(sscanf(r+pos,"%lf", &tmpd1) != 1)  return(-1);
	    srules->val = tmpd1;
	    break;
	case 'g':
	case 'G':
	    srules->range_type = RANGE_GREATER;
	    pos+=2;
	    if(pos > len) return (-1);
	    if(sscanf(r+pos,"%lf", &tmpd1) != 1)  return(-1);
	    srules->val = tmpd1;
	    break;
	case 'e':
	case 'E':
	    srules->range_type = RANGE_EQUAL;
	    pos+=2;
	    if(pos > len) return (-1);
	    if(sscanf(r+pos,"%lf", &tmpd1) != 1)  return(-1);
	    srules->val = tmpd1;
	    break;
	default:
	    srules->range_type = RANGE_LOW_HIGH;
	    if(sscanf(r+pos,"%lf-%lf", &tmpd1, &tmpd2) != 2)  return(-1);
	    srules->lowval = (tmpd1 < tmpd2? tmpd1: tmpd2);
	    srules->highval = (tmpd1 > tmpd2? tmpd1: tmpd2);
	    break;
    }

    Rulenum++;
    return (1);

}

int 
site_qualify (Site *s)
{
int i, ret;
Site_rules *srules;

    ret = 1;
    for(i=0; i<Rulenum; i++){
	srules = &(SR[i]);
	ret = site_in_rule(s, srules);
	if (ret == -1){
	    G_fatal_error("Bad match - site format inadequate for rule");
	}
	else if (ret == 0) 
	    return (ret);
    }
    return (ret);

}

/* THINK ABOUT mapping compar value to display object & attributes 
   (separate function) or just returning compar value in a double 

   Returns -1 on error, 1 if qualifies, 0 if it doesn't
   
*/

int 
site_in_rule (Site *s, Site_rules *srules)
{
double compar;


    if (srules->use_this == USE_DBL){
	if(srules->param > s->dbl_alloc || srules->param < 1)
	     return (-1);
	compar = s->dbl_att[srules->param-1];
    }
    else if (srules->use_this == USE_DIM){
	if(srules->param > (s->dim_alloc+2) || srules->param < 1)
	     return (-1);

	if(srules->param == 1)
	    compar = s->east;
	else if(srules->param == 2)
	    compar = s->north;
	else
	    compar = s->dim[srules->param - 3];
    }
    else if (srules->use_this == USE_CAT){
	switch (s->cattype){
	    case CELL_TYPE:
		compar = s->ccat;
		break;
	    case FCELL_TYPE:
		compar = s->fcat;
		break;
	    case DCELL_TYPE:
		compar = s->dcat;
		break;
	    default :
	        return (-1);
	}
    }
    else
	return(-1);

    switch (srules->range_type){
	case RANGE_LESS:
	    return (compar <= srules->val);
	case RANGE_GREATER:
	    return (compar >= srules->val);
	case RANGE_EQUAL:
	    return (compar == srules->val);
	default:   /* low_high */
	    return (compar >= srules->lowval && 
		    compar <= srules->highval);
    }
    

}
