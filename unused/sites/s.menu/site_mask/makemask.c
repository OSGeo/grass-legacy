
#include <string.h>
#include "vask.h"
#include "gis.h"

#define NLINES 10

int 
makemask (struct Categories *cats, char *mask)
{
    char name[NLINES][80] ;
    char next[20];
    char table[NLINES][3];
    char next_line[80];
    int i ;
    int atnum ;
    int line;
    int startcat ;
    int endcat ;


    startcat = 0;
    while (startcat >= 0 && startcat <= cats->num) 
    {
	V_clear() ;
	V_line(0 ,"SITE MASK");
	V_line(2 ,
"Please enter a '1' next to those categories you wish to include in the mask");

	endcat = startcat+NLINES <= cats->num+1 ? startcat+NLINES : cats->num+1 ;

	atnum = 0;
	line = 5;
	for (i=startcat; i < endcat; i++) {
	    sprintf (name[atnum], " %5d %-68.68s", i, G_get_cat((CELL)i, cats));
	    V_line  (line, name[atnum]) ;
	    if (mask[i])
		strcpy (table[atnum], "x");
	    else
		strcpy (table[atnum], "");
	    V_ques  (table[atnum], 's', line, 0, 1) ;
	    atnum++;
	    line++;
	}

	line += 2;
	*next = 0;
	if (endcat > cats->num)
	    strcpy (next, "end");
	else
	    sprintf (next, "%d", endcat);
	sprintf (next_line, "%*s%*s  (of %ld)", 26, "Next category: ",5,"",
		(long) cats->num);
	V_line (line, next_line);
	V_ques (next, 's', line, 26, 5);

	V_intrpt_ok();
	if(!V_call())
	    return 0;

	atnum = 0;
	for (i=startcat; i < endcat; i++) {
	    mask[i] = (table[atnum][0] != 0) ;
	    atnum++;
	}

	if (*next == 0) break;
	if (strcmp (next, "end") == 0) break;
	if (sscanf (next, "%d", &endcat) != 1)
		continue;
	if (endcat < 0)
	    endcat = 0;
	if (endcat > cats->num)
	{
	    endcat = cats->num - NLINES + 1;
	    if (endcat < 0) endcat = 0;
	}
	startcat = endcat ;
    }
    return 1;
}
