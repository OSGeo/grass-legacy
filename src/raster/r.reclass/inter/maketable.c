/*
 * Section of reclass that establishes the reclassification lookup table
 */

#include <string.h>
#include "gis.h"
#include "vask.h"
#include "r.reclass.h"

#define NLINES 10

int maketable ( struct Categories *cats, CELL *table,
    CELL min,CELL max,int MASK)
{
    long catnum[NLINES] ;
    char name[NLINES][80] ;
    char next[20];
    char tmp_buf[500];
    char next_line[80];
    static char **char_table;
    static int nalloc=0;
    int i ;
    int atnum ;
    int line;
    int startcat ;
    int endcat ;

    if(!nalloc)
	char_table = (char **) G_calloc((max-min+1), sizeof(char *));
    else
	char_table = (char **) G_realloc((char **) char_table, (max-min+1) * sizeof(char *));

/* Bugfix is i=nalloc-1 -> i=nalloc - LP 2/11/1999 */
    for(i=nalloc; i< max-min +1; i++)
	char_table[i] = G_malloc(5 * sizeof(char));

/* initialize the table to zero */
    for (i=0; i <= max - min; i++)
    {
	*char_table[i] = '0' ;
	table[i] = (CELL)0 ;
        sprintf(char_table[i], "%d", table[i]);
    }

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
	V_line (9, "  2   All values set to null (no data)");

	do
	{
	    V_intrpt_ok();
	    if (!V_call())
		exit(0);
	} while (set != 0 && set != 1 && set != 2);

	if (set==1)
	    for (i=0; i <= max - min; i++)
	    {
		table[i] = min + i; 
		sprintf(char_table[i], "%d", table[i]);
            }

	if (set==2)
	    for (i=0; i <= max - min; i++)
		sprintf(char_table[i], "null");
    }


    startcat = min;
    while (startcat >= min && startcat <= max) 
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

	endcat = startcat+NLINES <= max+1 ? startcat+NLINES : max+1 ;

	atnum = 0;
	line = 5;
	for (i=startcat; i < endcat; i++) {
	    catnum[atnum] = i ;
	    dots (G_get_cat((CELL)i,cats), name[atnum], 65);
	    V_line  (line, name[atnum]) ;
	    V_const (&catnum[atnum], 'l', line, 68, 5) ;
	    V_ques  (char_table[i-min], 's', line, 74, 5) ;
	    atnum++;
	    line++;
	}

	line += 2;
	*next = 0;
	if (endcat > max)
	    strcpy (next, "end");
	else
	    sprintf (next, "%d", endcat);
	sprintf (next_line, "%*s%*s  (%d thru %d)",
		26, "Next category: ",5,"",min, max);
	V_line (line, next_line);
	V_ques (next, 's', line, 26, 5);

	V_intrpt_ok();
	if(!V_call())
	    exit(0);

	if (*next == 0) break;
	if (strcmp (next, "end") == 0) break;
	if (sscanf (next, "%d", &endcat) != 1)
		continue;
	if (endcat < min)
	    endcat = min;
	if (endcat > max)
	{
	    endcat = max - NLINES + 1;
	    if (endcat < min) endcat = min;
	}
	startcat = endcat ;
    }
    if (MASK)
    {
	for (i = min ; i <= max; i++)
	    if (*char_table[i-min] != '0')
		table[i-min] = (CELL)1;
    }
    else
	for (i = min ; i <= max; i++)
	{
	    if (*char_table[i-min])
	    {
		fprintf (stdout,"%d %s\n", i, char_table[i-min]);
		if(sscanf(char_table[i-min], "%d", &table[i-min]) != 1)
		{
		    if(strncmp(char_table[i-min], "null", 4) == 0 ||
		       strncmp(char_table[i-min], "NULL", 4) == 0)
		       G_set_c_null_value((CELL *) &table[i-min], 1);
                    else 
		    { 
			sprintf(tmp_buf, "Illegal filed: %s!, assuming NULL", char_table[i-min]);
			G_warning(tmp_buf);
                    }
                }
            }
         }

	return 0;
}
