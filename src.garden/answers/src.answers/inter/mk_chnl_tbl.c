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

   function: mk_chnl_tbl
   called by:  get_chnl

   mk_chnl_tbl gives the user a worksheet to enter the parameters
   needed by answers for each chnl category found in the study 
   area. a table of categories are found by the function
   mk_cat_tbl, which fills the global structure cat_tbl.
   once the user completes the input of parameters, they are
   stored in the project directory (G_mapset()/answers/data/proj_name).
   two files are created in this directory; <chnl_data>, which can
   be read back in by this function to facilitate editing by the
   user; and chnl_predata, which is the data formated to serve as the
   section of the answers predata input. 
   
*/

#define NLINES 10
#include "answers.h"
static char *intro_screen[] = {
"",
"For each channel category identified in the watershed, ANSWERS requires",
"values for the parameters listed below. The Project Manager facilitates",
"preparation parameters by input into a table.", 
"",
"-----------------------------------------------------------------------",
"Channel Parameters for ANSWERS (see ANSWERS Users Manual for details)",
"-----------------------------------------------------------------------",
" 1   Channel width         (meters)",
" 2   Channel roughness     (Manning's n)",
"",
NULL};

mk_chnl_tbl()
{

    char next[20];
    char next_line[80];
    int i,
        err;
    int check_digit;
    char tmpname[123];
    char buf[200];
    int atnum;
    int line;
    int startcat;
    int endcat;
    FILE *data1_fp,
         *data2_fp;


/* cat_tbl[N].param[0-8] is an array to hold the data for chnl
   parameters ('N' is the index for nth cat found). there are 2
   values plus the category number as follows:

    0     1     2    
-------------------
  cat   width  roughness
  num            (n)

*/

    G_clear_screen();
    i = 0;
    while (intro_screen[i])
	printf("%s\n", intro_screen[i++]);
    printf("\n\n\n\n\n");

    if (complete[8] == 0)
        hit_return();
        
    if ((complete[8] > 0) &&
	(G_yes("Would you like to edit previously entered parameters?", 1)))
    {
    
/* read in data from last time */

	data1_fp = G_fopen_old(data_dir, "chnl_data", proj_mapset);
	if (!data1_fp)
	{
	    croak(1, "Could not open <chnl_data> file in project database.");
	}

/* loop thru for each cat in the mask or until an error */

	i = 0;
	while ((complete[8] > 0) && (++i <= cat_tbl[0].cat))
	{
/* scan in line of data */
	    err = fscanf(data1_fp, "%d %f %f\n",
	    &check_digit, &cat_tbl[i].param[1], &cat_tbl[i].param[2]);

/* check to see if we match the cat # stored and the cat # scanned in 
   we will allow user to continue (and we won't try to read any more)
   or they can go back to main menu, without changing anything. This may
   not gain them much unless they have the ability to find the data file
   and discover the problem. otherwise previously entered parameters is 
   gonesville */

	    if (cat_tbl[i].cat != check_digit)
	    {
		printf("\nWARNING: Expecting category <%d> but found category <%d>\n", 
		cat_tbl[i].cat, check_digit);
		strcpy(buf, "Categories found in channel layer do not match those\n");
		strcat(buf, "in the parameters saved in the project database.\n");
		strcat(buf, "You can continue, but previously saved parameters\n");
		strcat(buf, "will be lost.\n");
		croak(0, buf);
		complete[8] = 0;
	    }
	   
/* the next error check looks at the value returned from fscanf, which is the
   number of values found. if this doesn't check out, something is wrong and
   we can't continue to trust any of this. like the last error, if they
   ask to continue we just forget the old params and start them out fresh.
   otherwise, they can quit to the main menu, giving an oportunity to look
   at the old data file (chnl_data) before it is overwritten. this is not a
   much of consolation but so it goes */
   
	    if (err != 3)
	    {
		printf("\nWARNING: Expecting <3> data items but found <%d>\n", err);
		strcpy(buf, "Problem with previously stored parameters in project\n");
		strcat(buf, "database. You can continue, but previously saved \n");
		strcat(buf, "parameters will be lost.\n");
		croak(0, buf);
		complete[8] = 0;
	    }
	}
	fclose(data1_fp);
    }
    else
    {
/* either this is the first time through or user doesn't want to
   (or can't) edit data read in from previous time so set the flag */

	complete[8] = 0;
    }


/* this loop will cycle through as many times as needed
   to get the values for all the categoies we have found
   in the mask */

    startcat = 1;

LOOP:

    while (startcat >= 0 && startcat <= cat_tbl[0].cat)
    {
	V_clear();
	V_line(2, "          Channel Parameters Entry Worksheet");
	V_line(3, "--------------------------------------------------------------");

	endcat = startcat + NLINES <= cat_tbl[0].cat + 1 ? startcat + NLINES : cat_tbl[0].cat + 1;

	atnum = 0;
	line = 5;
	for (i = startcat; i < endcat; i++)
	{
	    V_line(line, "Category:           Width:       meters       Roughness:");
	    V_const(&cat_tbl[i].cat, 'l', line, 10, 4);
	    V_float_accuracy(1);
	    V_ques(&cat_tbl[i].param[1], 'f', line, 27, 4);
	    V_float_accuracy(3);
	    V_ques(&cat_tbl[i].param[2], 'f', line, 57, 4);

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

    if (G_yes("\n\n\nWould you like to change any of the parameters?", 0))
    {
	startcat = 1;
	goto LOOP;
    }

/* open and write parameters to data files in project directory
   "chnl_data" is used to store parameters in a form this function can read
   "chnl_predata" is the prepared section of the predata input for answers */

    data1_fp = G_fopen_new(data_dir, "chnl_data");
    if (!data1_fp)
    {
	sprintf(buf, "Could not create <chnl_data> file in project database.");
	croak(1, buf);
    }
    data2_fp = G_fopen_new(data_dir, "chnl_predata");
    if (!data2_fp)
    {
	sprintf(buf, "Could not create <chnl_predata> file in project database.");
	croak(1, buf);
    }

/* header lines for predata file  */

    fprintf(data2_fp, 
        "  CHANNEL SPECIFICATIONS FOLLOW\n");
    fprintf(data2_fp, " NUMBER OF TYPES OF CHANNELS =%3d\n", cat_tbl[0].cat);

/* write out params to both data files */

    for (i = 1; i <= cat_tbl[0].cat; i++)
    {
	fprintf(data1_fp, "%ld %.2f %.2f\n",
	    cat_tbl[i].cat, cat_tbl[i].param[1], cat_tbl[i].param[2]);

        if (cat_tbl[i].param[1] < 99)
        {
	fprintf(data2_fp,
	" CHANNEL%2d, WIDTH=%2.1f  M., ROUGHNESS COEFF.(N) =%0.3f\n",
	    i, cat_tbl[i].param[1], cat_tbl[i].param[2]);
        }
        else
        {
	fprintf(data2_fp,
	" CHANNEL%2d, WIDTH=%3.0f  M., ROUGHNESS COEFF.(N) =%0.3f\n",
	    i, cat_tbl[i].param[1], cat_tbl[i].param[2]);
        }
    }

    fclose(data1_fp);
    fclose(data2_fp);
    
/* extract data from input layer to ANSWERS input file */

    if(extract(0, chnl_layer, chnl_mapset, "in_chnl"))
    {
        complete[8] = 0;
        croak(1, "Failure to extract ANSWERS input data");
    }
    else
    {
        printf("\n\nANSWERS input data extraction complete.\n\n");
        complete[8] = 1;
    }

/* construct path to predata file in case the user wants a copy */

    strcpy(tmpname, G_location_path());
    strcat(tmpname, "/");
    strcat(tmpname, proj_mapset);
    strcat(tmpname, "/");
    strcat(tmpname, data_dir);
    strcat(tmpname, "/");
    strcat(tmpname, "chnl_predata");

/* see if the user wants a copy, if so make one */

    printf("\nParameters now stored in the project database.\n");
    if(G_yes("Would you like to review, copy or print this information?", 0))
        user_file(tmpname);
    return (0);
}
