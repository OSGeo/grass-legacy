/*
 * Section of reclass that establishes the reclassification lookup table
 */

#include "gis.h"

#define NLINES 10

maketable (cats, table, MASK)
    struct Categories *cats ;
    long *table;
{
    long catnum[NLINES] ;
    char name[NLINES][80] ;
    char next[20];
    char next_line[80];
    int i ;
    int atnum ;
    int line;
    int startcat ;
    int endcat ;

/* initialize the table to zero */
    for (i=0; i <= cats->num; i++)
	table[i] = 0 ;

/* reclass, ask user how table whould be initialized */
    if (!MASK)
    {
	int set;

	set = 0;
	V_clear();
	V_line (0, "Please indicate how you would like the reclass table initialized");
	V_ques (&set, 'i', 4, 15, 2);
	V_line (7, "  0   All values set to zero");
	V_line (8, "  1   All values set to the same category number");

	do
	{
	    V_intrpt_ok();
	    if (!V_call())
		exit(0);
	} while (set != 0 && set != 1);

	if (set)
	    for (i=1; i <= cats->num; i++)
		table[i] = i ;
    }


    startcat = 0;
    while (startcat >= 0 && startcat <= cats->num) 
    {
	V_clear() ;
	if (MASK)
	{
	    V_line ( 0, "  IDENTIFY THOSE CATEGORIES TO BE INCLUDED IN THE MASK" ) ;
	    V_line ( 2, "   OLD CATEGORY NAME                                                    CAT" ) ;
	    V_line ( 3, "                                                                        NUM" ) ;
	}
	else
	{
	    V_line ( 0, "   ENTER NEW CATEGORY NUMBERS FOR THESE CATEGORIES" ) ;
	    V_line ( 2, "   OLD CATEGORY NAME                                                OLD   NEW" ) ;
	    V_line ( 3, "                                                                    NUM   NUM" ) ;
	}

	endcat = startcat+NLINES <= cats->num+1 ? startcat+NLINES : cats->num+1 ;

	atnum = 0;
	line = 5;
	for (i=startcat; i < endcat; i++) {
	    catnum[atnum] = i ;
	    dots (G_get_cat((CELL)i,cats), name[atnum], 65);
	    V_line  (line, name[atnum]) ;
	    V_const (&catnum[atnum], 'l', line, 68, 5) ;
	    V_ques  (&table[i], 'l', line, 74, 5) ;
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
		(long)cats->num);
	V_line (line, next_line);
	V_ques (next, 's', line, 26, 5);

	V_intrpt_ok();
	if(!V_call())
	    exit(0);

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
    if (MASK)
	for (i = 0 ; i <= cats->num; i++)
	    if (table[i])
		table[i] = 1;
}
