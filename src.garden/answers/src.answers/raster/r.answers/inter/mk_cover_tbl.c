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

   function: mk_cover_tbl
   called by:  get_cover

   mk_cover_tbl gives the user a worksheet to enter the parameters
   needed by answers for each cover category found in the study 
   area. a table of categories are found by the function
   mk_cat_tbl, which fills the global structure cat_tbl.
   once the user completes the input of parameters, they are
   stored in the project directory (G_mapset()/answers/data/proj_name).
   two files are created in this directory; cover_data, which can
   be read back in by this function to facilitate editing by the
   user; and cover_predata, which is the data formated to serve as the
   section of the answers predata input. 
   
   this function is written with the assumption that the proper mask
   and region for the project are set. class 0 (no data), therefore is
   not considered in the table of parameters */


#define NLINES 10
#include "answers.h"
static char *intro_screen[] = {
"",
"For each land use identified in the watershed, ANSWERS requires",
"values for the parameters listed below. The Project Manager facilitates",
"preparation parameters by input into a table.", 
"",
"------------------------------------------------------------------------",
"Land Cover Parameters for ANSWERS (see ANSWERS Users Manual for details)",
"------------------------------------------------------------------------",
" 1  CROP- short (8 characters) description of land use and management",
"          (program will attempt to use map category description, if any)",
" 2  PIT - mm of potential rainfall interception by land cover",
" 3  PER - percentage of surface covered by specified land use",
" 4  RC  - roughness coefficient of the surface (a shape factor)",
" 5  HU  - mm of maximum roughness height of the surface profile",
" 6  N   - Manning's n (a measure of flow retardance of the surface)",
" 7  C   - relative erosiveness (function of time and USLE 'C' and 'P')",
"",
"------------------------------------------------------------------------",
"",
NULL};


mk_cover_tbl()
{

    char label[100][10];
    char next[20];
    char next_line[80];
    int j;
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

/* cat_tbl[N].param[0-7] is an array to hold the data for cover
   parameters ('N' is the index for nth cat found). there are 7
   values plus the category number as follows:

    0     1     2     3     4     5     6    7
--------------------------------------------------
  cat   CROP   PIT   PER    RC   HU     N    C
  num

*/

    for (i = 0; i < 100; i++)
	for (j = 0; j < 10; j++)
	    label[i][j] = 0;

    G_clear_screen();
    i = 0;
    while (intro_screen[i])
	printf("%s\n", intro_screen[i++]);
   if(complete[3] == 0)
       hit_return();

    if ((complete[3] > 0) &&
	(G_yes("Would you like to edit previously entered parameters?", 1)))
    {

/* read in data from last time */

	data1_fp = G_fopen_old(data_dir, "cover_data", proj_mapset);
	if (!data1_fp)
	{
	    croak(1, "Could not open <cover_data> file in project database.");
	}

	i = 0;

/* loop thru for each cat in the mask or until an error */

	while ((complete[3] > 0) && (++i <= cat_tbl[0].cat))
	{
/* scan in line of data */
	    err = fscanf(data1_fp, "%8c %d %f %f %f %f %f %f\n",
		label[i], &check_digit, &cat_tbl[i].param[1], &cat_tbl[i].param[2],
		&cat_tbl[i].param[3], &cat_tbl[i].param[4], &cat_tbl[i].param[5],
		&cat_tbl[i].param[6]);

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
		strcpy(buf, "Categories found in cover layer do not match those\n");
		strcat(buf, "in the parameters saved in the project database.\n");
		strcat(buf, "You can continue, but previously saved parameters\n");
		strcat(buf, "will be lost.\n");
		croak(0, buf);
		complete[3] = 0;
	    }
	   
/* the next error check looks at the value returned from fscanf, which is the
   number of values found. if this doesn't check out, something is wrong and
   we can't continue to trust any of this. like the last error, if they
   ask to continue we just forget the old params and start them out fresh.
   otherwise, they can quit to the main menu, giving an oportunity to look
   at the old data file (cover_data) before it is overwritten. this is not
   much of a consolation but so it goes */
   
	    if (err != 8)
	    {
		printf("\nWARNING: Expecting 8 data items but found %d\n", err);
		strcpy(buf, "Problem with previously stored parameters in project\n");
		strcat(buf, "database. You can continue, but previously saved \n");
		strcat(buf, "parameters will be lost.\n");
		croak(0, buf);
		complete[3] = 0;
	    }
	}
	fclose(data1_fp);
    }
    else
    {
/* either this is the first time through or user doesn't want to
   (or can't) edit data read in from previous time so set the flag */

	complete[3] = 0;
    }

/* copy cat text label (if any) to an array
   make sure that label is 8 char long or less, since the "crop"
   field n answer's input allows only that much */

    if (complete[3] == 0)
	for (i = 1; i <= cat_tbl[0].cat; i++)
	    G_strncpy(label[i], cat_tbl[i].label, 8);

/* this loop will cycle through as many times as needed
   to get the values for all the categories we have found
   in the mask */

    startcat = 1;

LOOP:

    while (startcat >= 0 && startcat <= cat_tbl[0].cat)
    {
	V_clear();
	V_line(0, "     Land Use and Surface Parameters Entry Worksheet");
	V_line(2,
"         Category  Potential    %     Rough   Rough   Manning's  Erosion");
	V_line(3,
" Map       Name    Intercept  Cover   Coeff.  Height     N       Constant");
	V_line(4,
"Category  (CROP)     (PIT)    (PER)    (RC)    (HU)     (N)      (USLE C)");
	V_line(5,
"----------------------------------------------------------------------------");

	endcat = startcat + NLINES <= cat_tbl[0].cat + 1 ? startcat + NLINES : cat_tbl[0].cat + 1;

	atnum = 0;
	line = 6;
	for (i = startcat; i < endcat; i++)
	{

	    V_const(&cat_tbl[i].cat, 'l', line, 2, 4);
	    V_ques(label[i], 's', line, 8, 8);
            V_float_accuracy(1);
	    V_ques(&cat_tbl[i].param[1], 'f', line, 20, 4); /* PIT */
            V_const("mm", 's', line, 25, 2);
            V_float_accuracy(0);
	    V_ques(&cat_tbl[i].param[2], 'f', line, 30, 3); /* PER */
            V_const("%", 's', line, 34, 1);
            V_float_accuracy(2);
	    V_ques(&cat_tbl[i].param[3], 'f', line, 39, 4); /* RC */
	    V_float_accuracy(0);
	    V_ques(&cat_tbl[i].param[4], 'f', line, 46, 4); /* HU */
            V_const("mm", 's', line, 51, 2);
	    V_float_accuracy(3);
	    V_ques(&cat_tbl[i].param[5], 'f', line, 55, 5); /* N */
	    V_float_accuracy(2);
	    V_ques(&cat_tbl[i].param[6], 'f', line, 67, 4); /* C */

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

/* even it this step was marked as complete before, we will mark
   as incomplete in case something weird happens */

    complete[3] = 0;

/* clean up text input */

    for (i = 1; i <= cat_tbl[0].cat; i++)
	G_strip(label[i]);

/* open and write parameters to data files in project directory
   "cover_data" is used to store parameters in a form this function can read
   "cover_predata" is the prepared section of the predata input for answers */

    data1_fp = G_fopen_new(data_dir, "cover_data");
    if (!data1_fp)
    {
	sprintf(buf, "Could not create <cover_data> file in project database.");
	croak(1, buf);
    }
    data2_fp = G_fopen_new(data_dir, "cover_predata");
    if (!data2_fp)
    {
	sprintf(buf, "Could not create <cover_predata> file in project database.");
	croak(1, buf);
    }

/* header lines for predata file  */

    fprintf(data2_fp, "  SURFACE ROUGHNESS AND CROP CONSTANTS FOLLOW\n");
    fprintf(data2_fp, " NUMBER OF CROPS AND SURFACES =%3d\n", cat_tbl[0].cat);

/* write out params to both data files */

/* note on error checking... the answers manual is extremely vague
   about the parameter values for crops in soils. it is hard to anticipate
   potential errors and/or legit inputs. however, we know how many
   and which cols answers will read from the predata, so we will make
   things fit. in mk_soil_tbl, we created a list of possible errs
   as a way of dealing with problems. here will will use croak
   to tell them as the values are written to the predata file */

    for (i = 1; i <= cat_tbl[0].cat; i++)
    {
	fprintf(data2_fp, "%5d CROP:%8s", cat_tbl[i].cat, label[i]);

/* if PIT is less than one, we will print 2 decimal places, and
   leave off space after CROP name to make room  else print 1 decimal */

        if (cat_tbl[i].param[1] < 10)
            fprintf(data2_fp, " PIT:%4.1f", cat_tbl[i].param[1]);
        else
        {
            if (cat_tbl[i].param[1] >= 100)
            {
                sprintf(buf,
                "value for <%s> <PIT %.2f> should not be greater than 99",
                label[i], cat_tbl[i].param[1]);
                croak(0, buf);
                printf("\nsetting to 99\n");
                cat_tbl[i].param[1] = 99;
            }
            fprintf(data2_fp, " PIT:%3.0f.", cat_tbl[i].param[1]);
        }

/* PER */

        if (cat_tbl[i].param[2] > 100)
        {
            sprintf(buf,
            "value for <%s> <PER %.0f> should not be greater than 100",
            label[i], cat_tbl[i].param[2]);
            croak(0, buf);
            printf("\nsetting to 100\n");
            cat_tbl[i].param[2] = 100;
        }
            fprintf(data2_fp, " PER:%3.0f", cat_tbl[i].param[2]);

/* RC and HU */


        if (cat_tbl[i].param[3] >= 1.0)
        {
            sprintf(buf,
            "value for <%s> <RC %.2f> should not be greater than 1",
            label[i], cat_tbl[i].param[3]);
            croak(0, buf);
            printf("\nsetting to .99\n");
            cat_tbl[i].param[3] = .99;
        }
        if (cat_tbl[i].param[4] >= 1000)
        {
            sprintf(buf,
            "value for <%s> <HU %.2f> should not be greater than 999",
            label[i], cat_tbl[i].param[4]);
            croak(0, buf);
            printf("\nsetting to 999\n");
            cat_tbl[i].param[4] = 999;
        }

	fprintf(data2_fp, " RC:%.2f HU:  %3.0f.",
        cat_tbl[i].param[3], cat_tbl[i].param[4]);

/* N */

        if (cat_tbl[i].param[5] >= 1.0)
        {
            sprintf(buf,
            "value for <%s> <N %.2f> should not be greater than 1",
            label[i], cat_tbl[i].param[5]);
            croak(0, buf);
            printf("\nsetting to .99\n");
            cat_tbl[i].param[5] = .99;
        }
	fprintf(data2_fp, " N:%.3f", cat_tbl[i].param[5]);

/* C */

        if (cat_tbl[i].param[6] >= 10.0)
        {
            sprintf(buf,
            "value for <%s> <C %.2f> should not be greater than 10",
            label[i], cat_tbl[i].param[6]);
            croak(0, buf);
            printf("\nsetting to 9.9\n");
            cat_tbl[i].param[6] = 9.9;
        }
    
        if (cat_tbl[i].param[6] < 1.0)
	    fprintf(data2_fp, " C:%.2f\n", cat_tbl[i].param[6]);
        else
	    fprintf(data2_fp, " C: %.1f\n", cat_tbl[i].param[6]);

/* print to the data file */

	fprintf(data1_fp, "%-8s %ld %.2f %.2f %.2f %4.1f %.3f %.2f\n",
	    label[i], cat_tbl[i].cat, cat_tbl[i].param[1], cat_tbl[i].param[2],
	    cat_tbl[i].param[3], cat_tbl[i].param[4], cat_tbl[i].param[5],
	    cat_tbl[i].param[6]);
    }

    fclose(data1_fp);
    fclose(data2_fp);

    if (G_yes("\n\n\nWould you like to change any of the parameters?", 0))
    {
	startcat = 1;
	goto LOOP;
    }

/* extract values from input layer into file for ANSWERS input */

    if(extract(0, cover_layer, cover_mapset, "in_cover"))
    {
        complete[3] = 0;
        croak(1, "Failure to extract ANSWERS input data");
    }
    else
    {
        printf("\n\nANSWERS inputp data extraction complete.\n\n");
        complete[3] = 1;
    }

/* construct path to predata file in case the user wants a copy */

    strcpy(tmpname, G_location_path());
    strcat(tmpname, "/");
    strcat(tmpname, proj_mapset);
    strcat(tmpname, "/");
    strcat(tmpname, data_dir);
    strcat(tmpname, "/");
    strcat(tmpname, "cover_predata");

/* see if the user wants a copy, if so make one */

    printf("\nParameters now stored in the project database.\n");
    if(G_yes("Would you like to review, copy or print this information?",0))
        user_file(tmpname);
    return (0);
}
