/**********************************************************************
 *
 *   G_edit_cats (name, cats, option)
 *      char *name
 *      struct Categories *cats 
 *
 *   Interactively prompts the user for category names for
 *   cats->num categories.  Uses screen oriented prompting through
 *   the visual_ask library.  Compile with $(VASK)
 *
 *   name is used for informatin on the screen only.
 *   No files are read or written
 *
 *   If option is 0, user can change number of cats
 *                1, user must initialize number of cats
 *               -1, user may not change number of cats
 *
 *
 *   Returns:
 *            -1 if user canceled the edit
 *             1 if ok
 *
 *   note:
 *      at present, this routine pretends to know nothing about the
 *      category label generation capabilities using the cats.fmt
 *      string. If it is necessary to let the user edit this
 *      a separate interface must be used
 **********************************************************************/
#include "gis.h"

#define NLINES 10

G_edit_cats (name, cats, option) 
    char *name;
    struct Categories *cats ;
{
    long incr ;
    long atnum ;
    long startcat ;
    long endcat ;
    long first_cat, last_cat;
    long catnum[NLINES] ;
    char buff[NLINES][80] ;
    char next[20];
    char next_line[80];
    char title[80];
    char msg1[80];
    char msg2[80];
    int line;

    if (cats->num < 0)
	option = 1;
    last_cat = (long)cats->num;
    if (option >= 0)
    {
	V_clear();
	V_line (1, msg1);
	V_line (2, msg2);
	if (option == 0)
	{
	    strcpy (msg1, "The current value for the highest category");
	    sprintf (msg2,"in [%s] is shown below", name);
	    V_line (3, "If you need to change it, enter another value");
	}
	else
	{
	    last_cat = 0;
	    strcpy (msg1, "Please enter the highest category value");
	    sprintf (msg2,"for [%s]", name);
	}
	V_line (10, "         Highest Category:");
	V_ques (&last_cat, 'l', 10, 28, 5);
	V_line (16, next_line);

	*next_line = 0;
	while (1)
	{
	    V_intrpt_ok();
	    if(!V_call())
		return -1;
	    if (last_cat >= 0)
		break;
	    sprintf (next_line, "** Negative values not allowed **");
	}
    }

    cats->num = last_cat;

    first_cat = 0;
    if (cats->count > 0 && cats->list[0].num < 0)
	first_cat = cats->list[0].num;

    *title = 0;
    if (cats->title != NULL)
	strcpy (title, cats->title);

    startcat = first_cat;
    sprintf(msg1, "[%s] ENTER NEW CATEGORY NAMES FOR THESE CATEGORIES", name ) ;
    while (1) 
    {
	V_clear() ;
	V_line ( 0, msg1 ) ;
	V_line ( 2, "TITLE: " ) ;
	V_line ( 3, "CAT   NEW CATEGORY NAME" ) ;
	V_line ( 4, "NUM" ) ;

	V_ques  (title, 's', 2, 8, 60) ;

	endcat = startcat+NLINES <= last_cat+1 ? startcat+NLINES : last_cat+1 ;

	line = 5;
	for (incr=startcat; incr < endcat; incr++) 
	{
	    atnum = incr-startcat ;
	    strcpy(buff[atnum], G_get_cat ((CELL)incr, cats)) ;
	    catnum[atnum] = incr ;
	    V_const (&catnum[atnum], 'l', line, 1, 3) ;
	    V_ques  (buff[atnum], 's', line, 5, 60) ;
	    line++;
	}

	line += 2;
	*next = 0;
	if (endcat > last_cat)
	    strcpy (next, "end");
	else
	    sprintf (next, "%ld", endcat);
	sprintf (next_line, "%*s%*s  (of %ld)", 26, "Next category: ",5,"",last_cat);
	V_line (line, next_line);
	V_ques (next, 's', line, 26, 5);

	V_intrpt_ok();
	if(!V_call())
	    return -1;

/* store new category name in structure */
	for (incr=startcat; incr < endcat; incr++) 
	{
	    atnum = incr-startcat ;
	    G_strip (buff[atnum]);
	    if (strcmp (buff[atnum], G_get_cat ((CELL)incr, cats)) != 0)
		G_set_cat ((CELL)incr, buff[atnum], cats);
	}

	if (*next == 0) break;
	if (strcmp (next, "end") == 0) break;
	if (sscanf (next, "%ld", &endcat) != 1)
		continue;
	if (endcat < first_cat)
	    endcat = first_cat;
	if (endcat > last_cat)
	{
	    endcat = last_cat - NLINES + 1;
	    if (endcat < 0) endcat = 0;
	}
	startcat = endcat ;
    }
    if (cats->title)
	free (cats->title);
    G_strip (title);
    cats->title = G_store (title);
    return (1) ;
}
