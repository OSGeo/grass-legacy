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

   function: mk_rain
   called by:  get_rain

   get_rain determines how many rain gauges to use. if more than one,
   a cell layer is used to depict the coverage area in the watershed
   of each gauge.

   mk_rain deals with the rain gauge data. the method of input is from
   a file with the user prepares. it should consist of two columns of 
   numbers (time in minutes and intensity in mm/hour). if multiple gauges
   are to be used, one input file is still used, data for each gauge are
   to be separated by "-1" flags.

   example input files:

     one gauge            two gauges
     _________            __________
       0   0.0               0  0.0
      10   3.4              11   .9     data for gauge 1
      20  10.0              25  7.5
      35  22.3              -1       <----- delimiter
      55   9.3               0  0.0
      67   4.0              15  6.2     data for gauge 2
     100   0.0              10  4.0

   it is likely that when answers is used on a watershed, several 
   rainfall events will be simulated. to facilitate this, this module
   has the ability to keep a database for rainfall events. rain data
   is kept in a separate directory in the answers database.
   
   the user is prompted for the rainfall event name. after the user's
   data file is read, two files are created; a record
   file (in the same format as the user's file which was read) and
   a "rain_predata" file, which is the gauge data formatted for input to
   the answers program. the record file if given the name of the rain
   fall event. rain data files are stored in the "rain" directory
   under the "data_dir" directory.
   
   to create a new rainfall event or to other wise change rainfall
   events, the user will run this module. get_rain sees that this
   step had been previously completed, it will offer previous settings
   as defaults. if the user had used one gauge, that will be the
   default. if a layer was used, that will be the default. if these are
   changed, all previously created rain data is removed, since it
   is unlikely we could hope data created for one set of gauges would
   match another. if the user accepts the defaults in get_rain, this
   module will ask if the user wishes to use a previous event. in this
   case we prompt for the name. if it exists in the rain data dir,
   we read in the data file and create a new corresponding predata file.
   (note, if the user simply wishes to obtain a copy of the raw data
   for editing, this module prompts "do you wish to save this information
   in a file?" the can do this, make changes, and run this module 
   again).
   
   if the user wishes to create a new rainfall event, the data is
   read in from a user file as before.
*/

#define NLINES 10
#include "answers.h"

mk_rain (gauges)
    int gauges;
{

    int f_data[10][55];
    float number;
    int i, j, err, mark;
    char buf[200];
    char tmpname[123];
    char tmpline[123];
    char dirname[100];
    FILE *data1_fp, *data2_fp, *data_in_fp;
    char *emalloc();

  /* set the rain data dir */

    sprintf(dirname, "/answers/data/%s/rain", proj_name);

  /* if gauges = 0 it is because our user asked to call a previous storm */

/*----*/
    if (gauges == 0)
    {

    printf("\nCurrently used rainfall event: %s\n", rain_event);
    if(!G_ask_old ("Enter the name of the rainfall event:\n", rain_event,
    dirname, "rain data storage"))
        return(0);

    data1_fp = G_fopen_old(dirname, rain_event, proj_mapset);
    data2_fp = G_fopen_new(data_dir, "rain_predata");
    if(!data1_fp || !data2_fp) 
    {
        sprintf(buf, "Could not create rain data files in project database.");
        croak(1, buf);
    }

  /* read in data from a previous rainfall event file  */

    j = i = 1;
    G_clear_screen();
    rain_mapset = emalloc((unsigned)(60));

/* recall pertinent info from the event stored in data file */

    fscanf(data1_fp, "%s", rain_layer);
    fscanf(data1_fp, " %s\n", rain_mapset);
    fscanf(data1_fp, "gauges: %d\n", &gauges);
    
    printf("\nReading rain data file <%s>\n", rain_event);
    if(gauges > 1)
        printf("Map: %s in %s\n", rain_layer, rain_mapset);
    printf("Number of gauges: %d\n", gauges);
    printf("\n  Data for gauge %d\n", i);
    printf("--------------------\n\n");
    while (1) 
    {
        err = fscanf(data1_fp, "%f", &number);
        if (err == EOF) 
        {
            f_data[i][j] = -1;
 /* set the next values to make saber happy */
            f_data[i][j+1] = 1;  
            break;
        }
        if (number == -1)  
        {
            /* put the -1 in the array to indicate the end */
            f_data[i][j] = (int) (number + 0.5);
 /* set the next values to make saber happy */
            f_data[i][j+1] = 1;  
            i++;
            if (i > gauges) 
            {
               fclose(data1_fp);
               fclose(data2_fp);
               G_remove(dirname, rain_event);
               G_remove(data_dir, "rain_predata");
               if (complete[5] > 0)
                   complete[5] = 2;

                sprintf(buf, "Data for more than %d gauges found.", gauges);
                croak(1, buf);
            }
            if (j % 2 == 0)
            {
                fclose(data1_fp);
                fclose(data2_fp);
                G_remove(dirname, rain_event);
                G_remove(data_dir, "rain_predata");
               if (complete[5] > 0)
                   complete[5] = 2;
                sprintf(buf, "Error reading data - odd number of datum.");
                croak(1, buf);
            }
            j = 1;
            printf("\n--------------------\n\n");
            hit_return(); 
            G_clear_screen();
            printf("\n\n  Data for gauge %d\n", i);
            printf("--------------------\n\n");
        }
        else 
        {
            f_data[i][j] = (int) (number + 0.5);
            printf("%7d   %c",  f_data[i][j], (j % 2== 0 ? '\n' : ' '));
            j++;
        }

    }
    printf("--------------------\n\n");

  /* construct the top line of the predata file.
     ANSWERS doesn't want to see more than an 8 char name for event */
    tmpname[0] = 0;
    strncat(tmpname, rain_event, 8);
    fprintf(data2_fp, " RAINFALL DATA FOR %d GAUGE(S) FOR EVENT OF:   %s\n",
    gauges, tmpname);
  
  /* write out the data to the predata file in
     the answers/data/rain directory */

    fprintf(data2_fp, " GAUGE NUMBER   R1\n");

    j = 1;
    for(i = 1; i <= gauges; i++) 
    {

        while(f_data[i][j] != -1) 
        {

            /* answers needs a 1 to mark the last line
               of a gauge's entry otherwise, a 0 */

            if(f_data[i][j+2] == -1)
                mark = 1;
            else
                mark = 0;

            /* see if we need a newline or not */

            if (j % 2 == 0) 
            {
                fprintf(data2_fp, "%10d\n", f_data[i][j]);
            }
            else 
            {
                fprintf(data2_fp, "%d  %7d", mark, f_data[i][j]);
            }
            j++;
        }
        j = 1;

        /* slip in a label for the next gauge, unless we
           have finished the last set of data */

        if(i < gauges){
            fprintf(data2_fp, " GAUGE NUMBER   R%d\n", i+1);
        }
    }
        fclose(data1_fp);
        fclose(data2_fp);
    }
/*----*/
    else {
/*----*/
 /* either this is the first time through or user is creating a new
    set of rain gauge data  */

    while(1){
        if(!G_ask_new ("Enter the name of the rainfall event:\n", rain_event,
        dirname, "rain data storage"))
            return(0);
        if(strlen(rain_event) > 8)
        {
            printf("\nANSWERS requires an event name of 8 letters or less.\n");
            sleep(3);
        }
        else
            break;
    }
    data1_fp = G_fopen_new(dirname, rain_event);
    data2_fp = G_fopen_new(data_dir, "rain_predata");
    if (!data1_fp || !data2_fp) {
        sprintf(buf,"Could not create rain data files in project database.");
        croak(1, buf);
    }

  /* get the name of the user's file where they have the rain gauge 
     data. the file needs to be in the current directory or we will
     need a full pathname */

    while (1) 
    {

	    fprintf(stderr,"\nEnter the name of your rainfall data file:\n");
	    fprintf(stderr,"Hit RETURN to cancel request.\n");
	    fprintf(stderr,">");
	    if (!G_gets(tmpline)) 
		continue;
            /* if we get nothing, cancel request */
	    if(sscanf(tmpline,"%s",tmpname) <= 0) 
	    {
               if (complete[5] > 0)
                   complete[5] = 2;
                return(0);
            }
            /* if we get something other than 1 string, ask again */
	    if(sscanf(tmpline,"%s",tmpname) != 1)
		continue;
	    G_strip (tmpname);
	    if (*tmpname == 0) 
                continue;
            fprintf (stderr,"<%s>\n", tmpname);
            data_in_fp = fopen (tmpname, "r");
            if (!data_in_fp) 
            {
                printf("\nCould not locate or open file: <%s>\n", tmpname);
                continue;
            }
        else
           break;
    }

  /* read in the user's data file while doing some quick error checks
     to see of the data is what we expect.  */

    j = i = 1;
    G_clear_screen();
    printf("\nReading rain data file <%s>\n", tmpname);
    printf("\n  Data for gauge %d\n", i);
    printf("--------------------\n\n");
    while (1) 
    {
        err = fscanf(data_in_fp, "%f", &number);
        if (err == EOF) 
        {
            f_data[i][j] = -1;
/* set the next value to make saber happy */   
            f_data[i][j+1] = 1;
            break;
        }
        if (err < 1) 
        {
            fclose(data1_fp);
            fclose(data2_fp);
            G_remove(dirname, rain_event);
            G_remove(data_dir, "rain_predata");
            if (complete[5] > 0)
                complete[5] = 2;
            sprintf(buf, "Something wrong in <%s>. Didn't find a number", tmpname);
            croak(1,buf);
        }
        if (number == -1)  
        {
            /* put the -1 in the array to indicate the end */
            f_data[i][j] = number;
 /* set the next values to make saber happy */
            f_data[i][j+1] = 1;  
            i++;
            if (i > gauges) 
            {
                fclose(data1_fp);
                fclose(data2_fp);
                G_remove(dirname, rain_event);
                G_remove(data_dir, "rain_predata");
                if (complete[5] > 0)
                    complete[5] = 2;
                sprintf(buf, "Data for more than %d gauges found.", gauges);
                croak(1, buf);
            }
            if (j % 2 == 0) 
            {
                sprintf(buf, "Error reading data - odd number of datum.");
                croak(1, buf);
                fclose(data1_fp);
                fclose(data2_fp);
                G_remove(dirname, rain_event);
                G_remove(data_dir, "rain_predata");
                if (complete[5] > 0)
                    complete[5] = 2;

            }
            j = 1;
            printf("\n--------------------\n\n");
            hit_return(); 
            G_clear_screen();
            printf("\n\n  Data for gauge %d\n", i);
            printf("--------------------\n\n");
        }
        else {
            f_data[i][j] = (int) (number + 0.5);
            printf("%7d   %c",  f_data[i][j], (j % 2== 0 ? '\n' : ' '));
            j++;
        }
    }

  /* what if there wasn't enough data to be found? */

    if (i != gauges) 
    {
        fclose(data1_fp);
        fclose(data2_fp);
        G_remove(dirname, rain_event);
        G_remove(data_dir, "rain_predata");
        if (complete[5] > 0)
             complete[5] = 2;
        sprintf(buf,
        "Data for %d rain gauges expected. %d found.", gauges, i);
        croak(1, buf);
    }

    printf("\n--------------------\n\n");


  /* construct the top line of the predata file.
     ANSWERS doesn't want to see more than an 8 char name for event */

    tmpname[0] = 0;
    strncat(tmpname, rain_event, 8);
    fprintf(data2_fp, " RAINFALL DATA FOR %d GAUGE(S) FOR EVENT OF:   %s\n",
    gauges, tmpname);
  
  /* write out the data to both a storage and reference file and a
     predata file in the answers/data/rain directory */

    fprintf(data2_fp, " GAUGE NUMBER   R1\n");
    fprintf(data1_fp, "%s %s\n", rain_layer, rain_mapset);
    fprintf(data1_fp, "gauges: %d\n", gauges);

    j = 1;
    for(i = 1; i <= gauges; i++) {

        while(f_data[i][j] != -1) {

            /* answers needs a 1 to mark the last line
               of a gauge's entry otherwise, a 0 */
            if(f_data[i][j+2] == -1)
                mark = 1;
            else
                mark = 0;
            /* see if we need a newline or not */
            if (j % 2 == 0) {
                fprintf(data1_fp, "%7d\n", f_data[i][j]);
                fprintf(data2_fp, "%10d\n", f_data[i][j]);
            }
            else {
                fprintf(data1_fp, "%7d ", f_data[i][j]);
                fprintf(data2_fp, "%d  %7d", mark, f_data[i][j]);
            }
            j++;
        }
        j = 1;
        /* slip in a label for the next gauge, unless we
           have finished the last set of data */
        if(i < gauges){
            fprintf(data1_fp, "-1\n");
            fprintf(data2_fp, " GAUGE NUMBER   R%d\n", i+1);
        }
    }

    fclose(data1_fp);
    fclose(data2_fp);
    }

/*----*/

    hit_return();
    G_clear_screen();
    printf("\n\nRain gauge data preparation complete.\n");

    if(extract_rain(gauges))
    {
        if (complete[5] > 0)
             complete[5] = 2;
        croak(1, "Failure to create ANSWERS input data");
    }
    else
    {
        printf("\n\nANSWERS input data creation complete.\n\n");
        complete[5] = 1;
    }

  /* construct path to data file in case the user wants a copy */
    strcpy(tmpname, G_location_path());
    strcat(tmpname, "/");
    strcat(tmpname, proj_mapset );
    strcat(tmpname, "/");
    strcat(tmpname, dirname);
    strcat(tmpname, "/");
    strcat(tmpname, rain_event);

  /* see if the user wants a copy, if so make one */
    if(G_yes("Would you like to review, copy or print this rain data?", 0))
        user_file(tmpname);
    return(0);
}


char *emalloc (n)
unsigned n;
{
    char *p, *malloc ();

    if ((p = malloc (n)) == NULL)
        {
        croak(1,"could not allocate string array");
        }
        return (p);
}

