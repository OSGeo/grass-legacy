/* %W% %G% */

/* 
    +---------------------------------------------------------+
    |            ANSWERS on GRASS Integration Project         |
    |  Developed in the Agriculture Engineering Department    |
    |                at Purdue University                     |
    |                        by                               |
    |           Chris Rewerts and Bernard Engel               |
    |                                                         |
    |   (c)Copyright, 1992 Purdue Research Foundation, West   |
    |   Lafayette, Indiana 47907. Permission to use, copy,    |
    |   modify, and distribute this software and its          |
    |   documentation for any purpose and without fee is      |
    |   hereby granted, provided that the above copyright     |
    |   notice appear in all copies.  This software is        |
    |   provided "as is" without express or implied warranty. |
    +---------------------------------------------------------+

   function: mk_bmp_tbl
   called by:  get_bmp

for each category found in grass waterway or field border
layer (indicated by the "num" being passed, get the width of the structure. 
from the user 
*/


#define NLINES 10
#include "answers.h"

mk_bmp_tbl(num)
    int num;
{

    char next[20];
    char next_line[80];
    int i,
        err;
    int check_digit;
    char filename[15];
    char buf[200];
    int atnum;
    int line;
    int startcat;
    int endcat;
    FILE *data1_fp;

    if (num == 2)
        strcpy(filename, "waterway");
    else
        strcpy(filename, "field_border");

    printf("\n\n\n");
    printf("For each %s category found in the watershed,\n",
    bmp_tbl[num].title);
    printf("enter the width of the structure.\n\n\n\n");

    if(bmp_tbl[num].set == 0)
        hit_return();
        
    if ((bmp_tbl[num].set == 1) &&
	(G_yes("\nUse previously entered widths?", 1)))
    {
    
/* read in data from last time */

	data1_fp = G_fopen_old(data_dir, filename, proj_mapset);
	if (!data1_fp)
	{
            sprintf(buf,
            "Problem opening <%s> in project data directory\n.", filename);
            sprintf(buf,
            "You can continue, but previous data will be lost.");
	    croak(0, buf);
            bmp_tbl[num].set = 0;
	}

/* loop thru for each cat in the mask or until an error */

	i = 0;
	while ((bmp_tbl[num].set == 1) && (++i <= cat_tbl[0].cat))
	{
/* scan in line of data */
	    err = fscanf(data1_fp, "%d %f\n",
		&check_digit, &cat_tbl[i].param[1]);

/* check to see if we match the cat # stored and the cat # scanned in 
   we will allow user to continue (and we won't try to read any more)
   or they can go back to main menu, without changing anything. This may
   not gain them much unless they have the ability to find the data file
   and discover the problem. otherwise previously entered parameters is 
   gonesville */

	    if (cat_tbl[i].cat != check_digit)
	    {
		printf(
                "\nERROR: Expecting category <%d> but found category <%d>\n", 
	        cat_tbl[i].cat, check_digit);
		sprintf(buf, "Categories found in <%s> do not match those\n",
                bmp_tbl[num].layer);
		strcat(buf, "in the parameters saved in the project database.\n");
		strcat(buf, "You can continue, but previously saved parameters\n");
		strcat(buf, "will be lost.\n");
		croak(0, buf);
		bmp_tbl[num].set = 0;
                startcat = 1;
                fclose(data1_fp);
                goto LOOP;
	    }
	   
/* the next error check looks at the value returned from fscanf, which is the
   number of values found. if this doesn't check out, something is wrong and
   we can't continue to trust any of this. like the last error, if they
   ask to continue we just forget the old params and start them out fresh.
   otherwise, they can quit to the main menu, giving an oportunity to look
   at the old data file before it is overwritten. this is not much of 
   a consolation but so it goes */
   
	    if (err != 2)
	    {
		printf("\nERROR: Expecting 2 data items but found %d\n", err);
		strcpy(buf, "Problem reading previously stored parameters in project\n");
		strcat(buf, "database. You can continue, but previously saved \n");
		strcat(buf, "parameters will be lost.\n");
		croak(0, buf);
		bmp_tbl[num].set = 0;
	    }
	}
	fclose(data1_fp);
    }
    else
    {
/* either this is the first time through or user doesn't want to
   (or can't) edit data read in from previous time so set the flag */

	bmp_tbl[num].set = 0;
    }


/* this loop will cycle through as many times as needed
   to get the values for all the categoies we have found
   in the mask */

    startcat = 1;

LOOP:

    while (startcat >= 0 && startcat <= cat_tbl[0].cat)
    {
	V_clear();
        V_const(bmp_tbl[num].title, 's', 2, 16, 20);
	V_line(2, "     Widths of ");
	V_line(3, "--------------------------------------------------------------");

	endcat = startcat + NLINES <= cat_tbl[0].cat + 1 ? startcat + NLINES : cat_tbl[0].cat + 1;

	atnum = 0;
	line = 5;
	for (i = startcat; i < endcat; i++)
	{
	    V_line(line, "Category:           Width:       meters");
	    V_const(&cat_tbl[i].cat, 'l', line, 10, 4);
	    V_float_accuracy(0);
	    V_ques(&cat_tbl[i].param[1], 'f', line, 27, 4);

	    atnum++;
	    line++;
	}

	line += 2;
	*next = 0;
	if (endcat > cat_tbl[0].cat)
	    strcpy(next, "end");
	else
	    sprintf(next, "%d", endcat);
	sprintf(next_line, "%*s%*s  (of %ld)", 26, "Next category: ", 5, "",
	    (long) cat_tbl[0].cat);
	V_line(line, next_line);
	V_ques(next, 's', line, 26, 5);

	V_intrpt_ok();
	if (!V_call())
	    return (0);

	if (*next == 0)
	    break;
	if (strcmp(next, "end") == 0)
	    break;
	if (sscanf(next, "%d", &endcat) != 1)
	    continue;
	if (endcat < 0)
	    endcat = 0;
	if (endcat > cat_tbl[0].cat)
	{
	    endcat = cat_tbl[0].cat - NLINES + 1;
	    if (endcat < 0)
		endcat = 0;
	}
	startcat = endcat;
    }

    if (G_yes("\n\n\nWould you like to change any of the widths?", 0))
    {
	startcat = 1;
	goto LOOP;
    }

/* open and write parameters to data files in project directory
   "filename" is used to store parameters in a form this function can read
*/

    data1_fp = G_fopen_new(data_dir, filename);
    if (!data1_fp)
    {
	sprintf(buf, "Could not create <%s> in project database.",
        filename);
	croak(1, buf);
    }

/* write out params to both data files */

    for (i = 1; i <= cat_tbl[0].cat; i++)
    {
	fprintf(data1_fp, "%ld %.0f\n",
        cat_tbl[i].cat, cat_tbl[i].param[1]);
    }

    fclose(data1_fp);
    bmp_tbl[num].set = 1;

    return (0);
}
