/* %G% %W% */
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

   function:  hydro_filter
   called by: answers_run

   create files from the "out_hydro" file that are easy to plot
   (by the d.linegraph program or other plotting programs).
   basically, I want each type of data in a separate file.
   
   */
#include "answers.h"

/* names of our five output files to be created */
char *hydro_file[] = {"hydro_time", "hydro_rain", "hydro_runoff",
"hydro_sed1", "hydro_sed2", "NULL"};

hydro_filter()

{
    int err;
    int i;
    int line_count;

    float time;
    float rain;
    float runoff;
    float sediment1;
    float sediment2;

    char line[180];
    char dirname[300];
    char readbuf[1024];
    FILE *output_fp;
    FILE *fp[6];
   
/* see if user wants to do this */
/* nah, it was obnoxious
    if (!G_yes("Do you want to format the outlet hydrograph data for plotting?", 1))
    {
        return(0);
    }
    printf("\n\nWorking...\n\n");

    */

    printf("\n\nFormatting outlet hydrograph data...\n\n");

    strcpy(dirname, G_location_path());
    strcat(dirname, "/");
    strcat(dirname, proj_mapset);
    strcat(dirname, "/");
    strcat(dirname, data_dir);


/* read answers output hydrograph file */

    output_fp = G_fopen_old(data_dir, "out_hydro", proj_mapset);
    if (!output_fp)
    {
        croak(1, "could not open ANSWERS hydrograph output file.");
    }
    
/* now open our five new files */

    for (i = 0; i <= 4; i++)
    {
        fp[i] = G_fopen_new(data_dir, hydro_file[i]);
        if (!fp[i])
        {
            sprintf(line, "could not open the new file <%s> in project database.",
            hydro_file[i]);
            croak(1, line);
        }
    }
    
 /* read past the first three lines of the file (text header) */

    for(i = 0; i < 3; i++)
    {
        fgets(readbuf,1024,output_fp);
    }
    
    err = line_count = 0;
    
/* read through the file, collect the information we need */
    while(1)
    {
        line_count++;
	err = fscanf(output_fp, "%f %f %f %f %f\n", &time, &rain, 
	&runoff, &sediment1, &sediment2);
	
	if(err == EOF)
	    break;

/* complain if we don't find our five numbers on a line */

	if (err != 5)
	{
	    sprintf(line,
	    "something awry in hydrograph data file (out_hydro: line %d)",
	    line_count);
	    croak(1, line);
	}
	
        fprintf(fp[0], "%.1f\n", time);
        fprintf(fp[1], "%.2f\n", rain);
        fprintf(fp[2], "%.4f\n", runoff);
        fprintf(fp[3], "%.0f\n", sediment1);
        fprintf(fp[4], "%.0f\n", sediment2);
    }
    
/* close the files */

    for (i = 0; i <= 4; i++)
    {
        fclose(fp[i]);
    }
    fclose(output_fp);

    return(0);
}

