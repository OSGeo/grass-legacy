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

   function: mk_soil_tbl
   called by:  get_soils

   mk_soil_tbl gives the user a worksheet to enter the parameters
   needed by answers for each soil category found in the study 
   area. a table of categories are found by the function
   mk_cat_tbl, which fills the global structure cat_tbl.
   once the user completes the input of parameters, they are
   stored in the project directory (G_mapset()/answers/data/proj_name).
   two files are created in this directory; soil_data, which can
   be read back in by this function to facilitate editing by the
   user; and soil_predata, which is the data formated to serve as the
   section of the answers predata input. 
   
   this function is written with the assumption that the proper mask
   and region for the project are set. class 0 (no data), therefore is
   not considered in the table of parameters */


#define NLINES 10
#include "answers.h"
static char *intro_screen[] = {
"",
"For each soil series identified in the watershed, ANSWERS requires",
"values for the parameters listed below. The Project Manager facilitates",
"preparation parameters by input into a table.", 
"",
"In addition to parameters which apply to given soil series, you will be",
"prompted for groundwater release fraction and tile drainage coefficient,",
"which will apply to the entire watershed.",
"",
"-----------------------------------------------------------------------",
"Soil Parameters for ANSWERS (see ANSWERS Users Manual for more details)",
"-----------------------------------------------------------------------",
" 1   TP  - total porosity (percent pore space volume of soil)",
" 2   FP  - field capacity (percent saturation)",
" 3   FC  - steady state infiltration rate (mm/hour)",
" 4   A   - difference between steady state and maximum infiltration ",
"           rate (mm/hour)",
" 5   P   - exponent in infiltration equation",
" 6   DF  - infiltration control zone depth (mm)",
" 7   ASM - antecedent soil moisture (percent saturation)",
" 8   K   - USLE 'K'",
"",
NULL};

mk_soil_tbl()
{

    char next[20];
    char next_line[80];
    int i,
        err;
    int limit;
    int check_digit;
    char tmpname[123];
    char buf[200];
    int atnum;
    int line;
    int startcat;
    int endcat;
    FILE *err_fp,
         *data1_fp,
         *data2_fp;
    float grndwtr_release = 0;
    float tile_coef = 0;


/* cat_tbl[N].param[0-8] is an array to hold the data for soil
   parameters ('N' is the index for nth cat found). there are 8
   values plus the category number as follows:

    0     1     2     3     4     5     6    7     8
-------------------------------------------------------
  cat    TP    FP     FC    A     P     DF  ASM    K
  num

*/

    G_clear_screen();
    i = 0;
    while (intro_screen[i])
	printf("%s\n", intro_screen[i++]);
    if(complete[2] == 0)
        hit_return();

    if ((complete[2] > 0) &&
	(G_yes("Would you like to edit previously entered parameters?", 1)))
    {

/* read in data from last time */

	data1_fp = G_fopen_old(data_dir, "soil_data", proj_mapset);
	if (!data1_fp)
	{
	    croak(1, "Problem opening <soil_data> file in project database.");
	}

        fscanf(data1_fp, "%f\n", &tile_coef);
        fscanf(data1_fp, "%f\n", &grndwtr_release);

/* loop thru for each cat in the mask or until an error */

	i = 0;
	while ((complete[2] > 0) && (++i <= cat_tbl[0].cat))
	{
/* scan in line of data */
	    err = fscanf(data1_fp, "%d %f %f %f %f %f %f %f %f\n",
		&check_digit, &cat_tbl[i].param[1], &cat_tbl[i].param[2],
		&cat_tbl[i].param[3], &cat_tbl[i].param[4], &cat_tbl[i].param[5],
		&cat_tbl[i].param[6], &cat_tbl[i].param[7], &cat_tbl[i].param[8]);

/* check to see if we match the cat # stored and the cat # scanned in 
   we will allow user to continue (and we won't try to read any more)
   or they can go back to main menu, without changing anything. This may
   not gain them much unless they have the ability to find the data file
   and discover the problem. otherwise previously entered parameters is 
   gonesville */

	    if (cat_tbl[i].cat != check_digit)
	    {
		printf("\nERROR: Expecting category %d but found category %d\n", 
		    cat_tbl[i].cat, check_digit);
		strcpy(buf, "Categories found in soil layer do not match those\n");
		strcat(buf, "in the parameters saved in the project database.\n");
		strcat(buf, "You can continue, but previously saved parameters\n");
		strcat(buf, "will be lost.\n");
		croak(0, buf);
		complete[2] = 0;
	    }
	   
/* the next error check looks at the value returned from fscanf, which is the
   number of values found. if this doesn't check out, something is wrong and
   we can't continue to trust any of this. like the last error, if they
   ask to continue we just forget the old params and start them out fresh.
   otherwise, they can quit to the main menu, giving an oportunity to look
   at the old data file (soil_data) before it is overwritten. this is not much
   of a consolation but so it goes */
   
	    if (err != 9)
	    {
		printf("\nERROR: Expecting 9 data items but found %d\n", err);
		strcpy(buf, "Problem reading previously stored parameters in project\n");
		strcat(buf, "database. You can continue, but previously saved \n");
		strcat(buf, "parameters will be lost.\n");
		croak(0, buf);
		complete[2] = 0;
	    }
	}
	fclose(data1_fp);
    }
    else
    {
/* either this is the first time through or user doesn't want to
   (or can't) edit data read in from previous time so set the flag */

	complete[2] = 0;
    }


/* this loop will cycle through as many times as needed
   to get the values for all the categoies we have found
   in the mask */

    startcat = 1;

LOOP:

    while (startcat >= 0 && startcat <= cat_tbl[0].cat)
    {
	V_clear();
	V_line(0, "           Soil Parameters Entry Worksheet");
        V_line(3,
"         Total    Field        Infiltration        Zone  Antecedent");
	V_line(4,
"  Map   Porosity Capacity ------ Constants ------  Depth  Moisture  USLE");
	V_line(5,
"Category  (TP)    (FP)    (FC)       (A)      (P)  (DF)    (ASM)    (K)");
	V_line(6, "-------------------------------------------------------------------------");

	endcat = startcat + NLINES <= cat_tbl[0].cat + 1 ? startcat + NLINES : cat_tbl[0].cat + 1;

	atnum = 0;
	line = 7;
	for (i = startcat; i < endcat; i++)
	{
	    V_float_accuracy(0);
	    V_const(&cat_tbl[i].cat, 'l', line, 2, 4);       /* cat # */
	    V_ques(&cat_tbl[i].param[1], 'f', line, 10, 3);  /* tp */
            V_const("%", 's', line, 13, 1);
	    V_ques(&cat_tbl[i].param[2], 'f', line, 18, 3);  /* fp */
            V_const("%", 's', line, 21, 1);
	    V_ques(&cat_tbl[i].param[3], 'f', line, 25, 3);  /* fc */
            V_const("mm/h", 's', line, 29, 4);
	    V_ques(&cat_tbl[i].param[4], 'f', line, 35, 3);  /* a */
            V_const("mm/h", 's', line, 39, 4);
	    V_float_accuracy(2);
	    V_ques(&cat_tbl[i].param[5], 'f', line, 45, 4);  /* p */
	    V_float_accuracy(0);
	    V_ques(&cat_tbl[i].param[6], 'f', line, 51, 3);  /* df */
            V_const("mm", 's', line, 55, 2);
	    V_ques(&cat_tbl[i].param[7], 'f', line, 60, 3);  /* asm */
            V_const("%", 's', line, 63, 1);
	    V_float_accuracy(2);
	    V_ques(&cat_tbl[i].param[8], 'f', line, 67, 4);  /* k */

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

/* get values for groundwater release fraction and subsurface drainage */

    V_clear();
    V_line(2, "The next two constants are used to help describe the contribution");
    V_line(3, "of subsurface drainage to the total water yield in the watershed.");
    V_line(5, "-The tile drainage coefficient indicates the design coefficient of");
    V_line(6, " tile drains in those areas designated as having tile drainage.");
    V_line(8, "-The groundwater release fraction is measure of the contribution");
    V_line(9, " of lateral groundwater movement or interflow to total runoff.");
    V_line(11, "     Tile drainage coefficient: xxxxx mm/day");
    V_line(12, "     (suggested range: 6.4 to 12.7 mm/day)");
    V_line(14, "     Groundwater release fraction: xxxx");
    V_line(15, "      (suggested range: 0.0 to 0.01)");
    V_line(20, "                   See ANSWERS manual for more details.");
    V_float_accuracy(2);
    V_ques(&tile_coef, 'f', 11, 32, 5);
    V_float_accuracy(3);
    V_ques(&grndwtr_release, 'f', 14, 35, 5);

    V_intrpt_ok();
    V_intrpt_msg("RETURN TO MAIN MENU");
    if (!V_call())
        return (0);

/* check for limits - this is a very unsophisticated way of checking 
   for valid parameter ranges. For the most part, my ranges will make
   sure that answers won't bomb and that I can be sure that I get the
   proper numerical format for the predata file and that is about it.
   the user can hang it they plug in stupid numbers. some day an
   ambituous masters student can come along and design a AI
   application to come up with the parameters. until then: */

    limit = 0;
/* we'll make a file to write the limit errors to... this way the
   list can be long and we can use 'more' to display and better yet,
   we can give them a file with the list of errors for reference */

    err_fp = G_fopen_new(data_dir, "err_tmp");
    if (!err_fp)
    {
	sprintf(buf, "Could not create <err_tmp> file in project database.");
	croak(1, buf);
    }
    limit = 0;
    fprintf(err_fp, "Soil Parameter Value Limit Check List\n\n");
    fprintf(err_fp, "Project: %s   Soil layer: %s in %s\n\n",
    proj_name, soil_layer, soil_mapset);
    fprintf(err_fp, "Soil parameter values are given a very liberal range\n");
    fprintf(err_fp, "By the ANSWERS on GRASS project manager. \n");
    fprintf(err_fp, " - All values must be greater than zero. \n");
    fprintf(err_fp, " - Values for 'P' and 'K' must also be less than 1\n");
    fprintf(err_fp, "\n");
    fprintf(err_fp, "The following soil parameter values do not fall within these\n");
    fprintf(err_fp, "limits and should be revised before attempting to run ANSWERS:\n\n");

    for (i = 1; i <= cat_tbl[0].cat; i++)
    {
        if (cat_tbl[i].param[1] <= 0)
        {
            fprintf(err_fp, "soil category: %3d   TP: %.0f\n",
            cat_tbl[i].cat, cat_tbl[i].param[1]);
            limit =1;
        }
        if (cat_tbl[i].param[2] <= 0)
        {
            fprintf(err_fp, "soil category: %3d   FP: %.0f\n",
            cat_tbl[i].cat, cat_tbl[i].param[2]);
            limit =1;
        }
        if (cat_tbl[i].param[3] <= 0)
        {
            fprintf(err_fp, "soil category: %3d   FC: %.0f\n",
            cat_tbl[i].cat, cat_tbl[i].param[3]);
            limit =1;
        }
        if (cat_tbl[i].param[4] <= 0)
        {
            fprintf(err_fp, "soil category: %3d    A: %.0f\n",
            cat_tbl[i].cat, cat_tbl[i].param[4]);
            limit =1;
        }
        if ((cat_tbl[i].param[5] <= 0) || (cat_tbl[i].param[5] >= 1))
        {
            fprintf(err_fp, "soil category: %3d    P: %.2f\n",
            cat_tbl[i].cat, cat_tbl[i].param[5]);
            limit =1;
        }
        if (cat_tbl[i].param[6] <= 0)
        {
            fprintf(err_fp, "soil category: %3d   DF: %.0f\n",
            cat_tbl[i].cat, cat_tbl[i].param[6]);
            limit =1;
        }
        if (cat_tbl[i].param[7] <= 0)
        {
            fprintf(err_fp, "soil category: %3d  ASM: %.0f\n",
            cat_tbl[i].cat, cat_tbl[i].param[7]);
            limit =1;
        }
        if ((cat_tbl[i].param[8] <= 0) || (cat_tbl[i].param[8] >= 1))
        {
            fprintf(err_fp, "soil category: %3d    K: %.2f\n",
            cat_tbl[i].cat, cat_tbl[i].param[8]);
            limit =1;
        }
    }
    fclose(err_fp);
    if (limit == 1)
    {
/* construct path to predata file in case the user wants a copy */

        strcpy(tmpname, G_location_path());
        strcat(tmpname, "/");
        strcat(tmpname, proj_mapset);
        strcat(tmpname, "/");
        strcat(tmpname, data_dir);
        strcat(tmpname, "/");
        strcat(tmpname, "err_tmp");
        sprintf(buf, "more %s", tmpname);
        G_system(buf);

/* see if the user wants a copy, if so make one */

        printf("\n");
        if(G_yes("Would you like to review, copy or print this information?",0))
            user_file(tmpname);
    }
    G_remove(data_dir, "err_tmp");
    if (G_yes("\n\n\nWould you like to change any of the parameters?", 0))
    {
	startcat = 1;
	goto LOOP;
    }

/* open and write parameters to data files in project directory
   "soil_data" is used to store parameters in a form this function can read
   "soil_predata" is the prepared section of the predata input for answers */

    data1_fp = G_fopen_new(data_dir, "soil_data");
    if (!data1_fp)
    {
	sprintf(buf, "Could not create <soil_data> file in project database.");
	croak(1, buf);
    }
    data2_fp = G_fopen_new(data_dir, "soil_predata");
    if (!data2_fp)
    {
	sprintf(buf,"Could not create <soil_predata> file in project database.");
	croak(1, buf);
    }

/* header lines for predata file  */

    fprintf(data2_fp, 
        "  SOIL INFILTRATION, DRAINAGE AND GROUNDWATER CONSTANTS FOLLOW\n");
    fprintf(data2_fp, " NUMBER OF SOILS=%4d\n", cat_tbl[0].cat);

/* write out params to both data files */

    fprintf(data1_fp, "%1.2f\n", tile_coef);
    fprintf(data1_fp, "%0.3f\n", grndwtr_release);

    for (i = 1; i <= cat_tbl[0].cat; i++)
    {
	fprintf(data1_fp, "%ld %.0f %.0f %.0f %.0f %.2f %.0f %.0f %.2f\n",
	    cat_tbl[i].cat, cat_tbl[i].param[1], cat_tbl[i].param[2],
	    cat_tbl[i].param[3], cat_tbl[i].param[4], cat_tbl[i].param[5],
	    cat_tbl[i].param[6], cat_tbl[i].param[7], cat_tbl[i].param[8]);
	fprintf(data2_fp,"S %4d TP:%3.0f   FP:%3.0f   FC:%5.1f   A:%5.1f  P:%3.2f   DF:%5.1f   ASM:%3.0f K: %3.2f\n",
	    i, cat_tbl[i].param[1], cat_tbl[i].param[2],
	    cat_tbl[i].param[3], cat_tbl[i].param[4], cat_tbl[i].param[5],
	    cat_tbl[i].param[6], cat_tbl[i].param[7], cat_tbl[i].param[8]);
    }
    fprintf(data2_fp, "DRAINAGE COEFFICIENT FOR TILE DRAINS =  %1.2f MM/24HR\n",
        tile_coef);
    fprintf(data2_fp, "GROUNDWATER RELEASE FRACTION =      %0.3f\n",
        grndwtr_release);

    fclose(data1_fp);
    fclose(data2_fp);

/* extract data from input layer to an ANSWERS input file */

    if(extract(1, soil_layer, soil_mapset, "in_soil"))
    {
        complete[2] = 0;
        croak(1, "Failure to extract ANSWERS input data");
    }
    else
    {
        printf("\n\nANSWERS input data extraction complete.\n\n");
        complete[2] = 1;
    }

/* construct path to predata file in case the user wants a copy */

    strcpy(tmpname, G_location_path());
    strcat(tmpname, "/");
    strcat(tmpname, proj_mapset);
    strcat(tmpname, "/");
    strcat(tmpname, data_dir);
    strcat(tmpname, "/");
    strcat(tmpname, "soil_predata");

/* see if the user wants a copy, if so make one */

    printf("\nParameters now stored in the project database.\n");
    if(G_yes("Would you like to review, copy or print this information?",0))
        user_file(tmpname);
    return (0);
}
