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

   function:  answers_input
   called by: menu 

   This function compiles all the information gathered by the
   project manager into an ANSWERS input file. 
   All other input-gathering steps must be complete for the project
   (even the optional inputs, at this time). Each step should have
   created the appropriate "predata" or "element" file in the
   $LOCATION/answers/PROJECT_NAME/data dir. The predata files are
   "rain_predata", "soil_predata", "cover_predata", and "chnl_predata".
   The element data, or data for each cell in the watershed, is 
   gathered by each step when that step is run. The step creates a
   file with data for each element on a line. These files are
   "in_row_col", "in_elev", "in_chnl", "in_soil", "in_cover",
   "in_rain", "in_tile", "in_chnl_slp", and "in_bmp". Thus, this
   program puts all this information together, hopefully in the
   format that ANSWERS expects.

   */
#include "answers.h"

char *file_list[] = {"in_row_col", "in_elev", "in_chnl", "in_soil",
"in_cover", "in_rain", "in_tile", "in_chnl_slp", "in_bmp", "NULL"};

answers_input()

{
    int i;
    int err;
    int m = 0;
    int row, col;
    int slope;
    int aspect;
    int chnl;
    int chnl_slp;
    int do_chnl_slp;
    int do_bmp;
    int soil;
    int cover;
    int tile;
    int bmp;
    int bmp_width;
    char line[180];
    char rain[4];
    FILE *fp;
    FILE *in_fp[10];

/* check to see if we have an eligible contestant to run this step */

    for(i = 1; i < 11; i++)
    {
       if (complete[i] !=1)
       {
           if (m == 0)
           {
               printf("ANSWERS input cannot yet be created. The following\n");
               printf("must be completed: \n\n");
               m = 1;
           }
           printf("Step %d: %s\n", i, step[i]);
       }
    }
    if (m == 1)
    {
       printf("\n\n\n\n\n");
       hit_return();
       return(0);
    }

/* if this step is marked as complete, we have no need to create
   another input file, but I will ask anyway, what the hell */

    if ((complete[11] == 1)
     && (!G_yes("Create a new ANSWERS input file?", 0)))
    {
        answers_run();
        return(0);
    }

    printf("\n\nCreating input file for ANSWERS\n\n");

    complete[11] = 0;

    fp = G_fopen_new( data_dir, "answers_input");
    if ( !fp )
    {
     croak(1,"Could not create file <answers_input> in project database");
    }

    fprintf(fp, " ANSWERS Input File for %s Project\n", proj_name);
    fprintf(fp,
    " METRIC units are used on input/output                   PRINT\n");

    printf("   reading rain gauge predata...");
    err = read_file("rain_predata", fp);
    if (err == -1)
    {
       croak(1, "Failure to read <rain_predata>. Run that step again.");
    }

    printf("\n\n   reading soils predata...");
    err = read_file("soil_predata", fp);
    if (err == -1)
    {
       croak(1, "Failure to read <soil_predata>. Run that step again.");
    }

    printf("\n\n   reading cover predata...");
    err = read_file("cover_predata", fp);
    if (err == -1)
    {
       croak(1, "Failure to read <cover_predata>. Run that step again.");
    }

    printf("\n\n   reading channel predata...");
    err = read_file("chnl_predata", fp);
    if (err == -1)
    {
       croak(1, "Failure to read <chnl_predata>. Run that step again.");
    }

    printf("\n\ncollecting and preparing element data...\n\n");

    fprintf(fp, "  ELEMENT SPECIFICATIONS FOR %s Project \n", proj_name);
    fprintf(fp, " EACH ELEMENT IS %5.1fM  SQUARE\n", proj_resolution);
    fprintf(fp, " OUTLET FROM ROW %4d COLUMN %4d\n", out_row, out_col);

/* open the input files */

    for (i = 0; i < 7; i ++)
    {
        in_fp[i] = G_fopen_old( data_dir, file_list[i], G_mapset());
        if ( !in_fp[i] )
        {
         sprintf(line,"Could not open file <%s> in project database.\n", 
         file_list[i]);
         strcat(line,"Perhaps you should run the appropriate step again to produce the file.");
         croak(1, line);
        }
    }
/* since chnl_slp is optional, we check to see if it is used
   before attempting to open */

    if(strcmp(chnl_slp_layer, "none") == 0)
        do_chnl_slp = 0;
    else
    {
        do_chnl_slp = 1;
        in_fp[7] = G_fopen_old( data_dir, file_list[7], G_mapset());
        if ( !in_fp[7] )
        {
         sprintf(line,"Could not open file <%s> in project database.\n", 
         file_list[7]);
         strcat(line,"Perhaps you should run that step again to produce the file.");
         croak(1, line);
        }
    }
/* since BMPs are optional, check to see if the input file 
   exists before attempting to open it */

    if(G_find_file(data_dir, "in_bmp", G_mapset()) == NULL)
        do_bmp = 0;
    else
    {
        do_bmp = 1;
        in_fp[8] = G_fopen_old( data_dir, file_list[8], G_mapset());
        if ( !in_fp[8] )
        {
         sprintf(line,"Could not open file <%s> in project data directory.\n", 
         file_list[8]);
         strcat(line,"Perhaps you should run the appropriate step again to produce the file.");
         croak(1, line);
        }
    }

    fprintf(stderr, "Percent complete: ");

/* each input file has a line for each cell in the watershed.
   loop through as we create the input file element info.
   to complicate things, not every type of input will have
   something for every element, or we may have to check to see
   how many spaces will be needed for a number, or some inputs
   will not be used since they are optional */

    for (i = 1; i <= cells_in_wshd; i++)
    {
        percent( i, cells_in_wshd, 5);

        fscanf(in_fp[0], "%d %d\n", &row, &col);
        fscanf(in_fp[1], "%d %d\n", &slope, &aspect);
        fscanf(in_fp[2], "%d\n", &chnl);
        fscanf(in_fp[3], "%d\n", &soil);
        fscanf(in_fp[4], "%d\n", &cover);
        fscanf(in_fp[5], "%s\n", rain);
        fscanf(in_fp[6], "%d\n", &tile);
        if (do_chnl_slp)
            fscanf(in_fp[7], "%d\n", &chnl_slp);
        else
            chnl_slp = 0;
        if (do_bmp)
            fscanf(in_fp[8], "%d %d\n", &bmp, &bmp_width);
         else
             bmp = 0;
        fprintf(fp,"%3d%3d", row, col);
        if(i == cells_in_wshd)
            fprintf(fp," 9");
        else
            fprintf(fp,"  ");
        fprintf(fp,"%3d %3d", slope, aspect);
        if(chnl < 1)
            fprintf(fp,"  ");
        else
            fprintf(fp,"%2d", chnl);  
        if((soil < 10) && (chnl > 0))
           fprintf(fp,"0%d", soil);
        else
           fprintf(fp,"%2d", soil);
        fprintf(fp,"  %2d   ", cover);
        fprintf(fp,"%s", rain);
        if(tile == 1)
            fprintf(fp," TILE");
        else
            fprintf(fp,"     ");
        if(chnl_slp > 0)
            fprintf(fp," %3d ", chnl_slp);
        else
            fprintf(fp,"     ");
        if(bmp > 0)
        {
            fprintf(fp,"%2d%4d\n", bmp, bmp_width);
        }
        else
            fprintf(fp,"\n");
    }  

/* close files */

    for (i = 0; i < 7; i ++)
    {
        fclose(in_fp[i]);
    }
    if (do_chnl_slp)
        fclose(in_fp[7]);
    if (do_bmp)
        fclose(in_fp[8]);
    fclose(fp);

    printf("\n\nANSWERS input file created.\n\n");
    complete[11] = 1;

    answers_run();
    return(0);
}
/*------------------------------------------------*/
/* this function is used to read the predata files
   into the answers_input file being created. Since the
   predata files should be in the correct format, we can
   read them in char for char as we find them     */

read_file(in_name, out_fp)
 
    char in_name[30];
    FILE *out_fp;
{
    int c;
    int i = 0;
    char line[120];
    FILE *in_fp;

    in_fp = G_fopen_old( data_dir, in_name, G_mapset());
    if ( !in_fp )
    {
     sprintf(line,"Could not open file <%s> in project database.\n", in_name);
     strcat(line,"Perhaps you should run the appropriate step again to produce the file.");
     croak(1, line);
    }

    while(( c = getc(in_fp)) != EOF)
    {
        putc(c, out_fp); 
        if(c == '\n')
            i++;
    }

    fclose(in_fp);
    return(i);
}

