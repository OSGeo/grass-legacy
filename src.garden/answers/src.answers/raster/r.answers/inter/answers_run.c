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

   function:  answers_run
   called by: answers_input

   */
#include "answers.h"

    char *key_word[] = {"YIELD", "INDIVIDUAL", "CHANNEL", "NULL"};
    char *outfile[] = {"out_text", "out_hydro", "out_sediment", "out_chnl",
    "NULL"};

answers_run()

{
    int err_exist;
    int c;
    int i;
    int line_ct1;
    int line_ct2;
    int output_type;
    int check_pt = 0;
    char line[180];
    char dirname[120];
    char *err_report_file;
    char wd_buf[80];
    char buf[91];
    FILE *err_report;
    FILE *output_fp;
    FILE *fp[4];
   
/* double check to see if we have an eligible contestant to run this step */

   if (complete[11] !=1)
   {
        printf("This step cannot be executed until an ANSWERS input file\n");
        printf("is completed (by running Step 11).\n\n");
        printf("\n\n\n\n\n\n\n\n\n\n");
        hit_return();
        return(0);
    }

    if (G_find_file (data_dir, "answers_input", proj_mapset) == NULL)
    {
        printf("\nNo input file found. Run Step 11 again to build one.\n");
        printf("\n\n\n\n\n\n\n\n\n\n");
        complete[11] = 2;
        hit_return();
        return(0);
    }
    if (!G_yes("Shall we run ANSWERS?", 1))
    {
        return(0);
    }
/*
*/

    printf("\n\nRunning...\n\n");

    strcpy(dirname, G_location_path());
    strcat(dirname, "/");
    strcat(dirname, proj_mapset);
    strcat(dirname, "/");
    strcat(dirname, data_dir);

    sprintf(line,
    "cd %s; %s/etc/answers/answers < answers_input 1> answers_output 2> answers_error",
    dirname, G_gisbase() );
   
    if (G_system(line) != 0)
    {
        printf("\n\nProgram could not execute ANSWERS\n\n");
        printf("Perhaps the program 'answers' is not available\n\n");
        complete[11] = 2;
        hit_return();
        return(0);
    }
    
/*
*/
    G_clear_screen();
    err_report_file = G_tempfile();
    err_report = fopen (err_report_file, "w");
    if (!err_report)
    {
        croak(1, "unable to create and open tempfile");
    }
    fprintf(err_report,"     ANSWERS on GRASS %s Project\n\n", proj_name);
    fprintf(err_report,"The following errors were reported by ANSWERS\n\n");
    err_exist = read_file("answers_error", err_report);
    fclose(err_report);
    if (err_exist > 0)
    {
        sprintf(line, "more %s", err_report_file);
        G_system(line);
        if(G_yes("Would you like to review, copy or print this information?",0))
            user_file(err_report_file);
        printf("\n\n");
        if (!G_yes("shall we attempt to process the ANSWERS output?", 1))
        {
            return(0);
        }
    }
    else
    {
        printf("\n\nNo errors were reported while ANSWERS was running...\n\n");
    }
    unlink(err_report_file);

    printf("\n\nProcessing ANSWERS output...\n\n");

/* now read answers output file */

    output_fp = G_fopen_old(data_dir, "answers_output", proj_mapset);
    if (!output_fp)
    {
        croak(1, "could not open ANSWERS output file <answers_output>.");
    }
    for (i = 0; i <= 3; i++)
    {
        fp[i] = G_fopen_new(data_dir, outfile[i]);
        if (!fp[i])
        {
            sprintf(line, "could not open the new file <%s> in project database.",
            outfile[i]);
            croak(1, line);
        }
    }

/* at this point, the output of answers is in the file
   "answers_output". the following while loop it designed to sift
   through this and separate the output into the destination
   files described here:

   1. out_text - the verbose printout reiterating the input 
                 params and watershed charateristics, etc
   2. out_hydro  the outlet hydrographs. at this point, I am
                 assuming the user will use some means to plot
                 or analyze this data, so I will leave the
                 headers intact.
   3. out_sediment the sediment loss/deposition for individual
                 cell elements. we strip off headers and send
                 the summary lines to out_text. the numbers will
                 be converted to new map layers.
   4. out_chnl   channel deposition data. will be made into map
                 layer.

   As a side note, I should mention it was very tempting to simply
   modify the ANSWERS fortran code and make this part superfluous.
*/


    i = 0;
    output_type = 0;
    line_ct1 = 0;
    line_ct2 = 0;

/* read answers output file one char at a time. collect a line's
   worth of data in the char array buf */

    while((c = getc(output_fp)) != EOF)
    {
        buf[i++] = c;
        if( c == '\n' )
        {
            buf[i] = '\0';
            i = 0;

/* grab first word on line, to help us know where we are */
            wd_buf[0] = '\0';
            sscanf(buf, "%s", wd_buf);

/* see if we find a key word, to indicate where we are
   so we can increment our way thru the key word list, and
   correspondingly thru the array of destination file pointers */

            if (strcmp(wd_buf, key_word[output_type]) == 0)
                output_type++;

/* grab the output hydrograph data. the lines starting with
   the words RUNOFF and AVERAGE we will send to the out_text file */

            if (output_type == 1)
            {
                if((strcmp(wd_buf, "RUNOFF") == 0) ||
                  (strcmp(wd_buf, "AVERAGE") == 0)) 
                {
                    fprintf(fp[0], "%s", buf);
                    printf("%s", buf);
                    continue;
                }
            }

/* grab the element sedimentation data. the lines starting with
   the words MAX and STD. we will send to the out_text file,
   top three lines we scrap */

            if (output_type == 2)
            {
                if(line_ct1++ < 3)
                    continue;
                if((strcmp(wd_buf, "MAX") == 0) || 
                (strcmp(wd_buf, "STD.") == 0))
                {
                    fprintf(fp[0], "%s", buf);
                    printf("%s", buf);
                    continue;
                }
            }

/* scrap top 2 lines of channel deposition data */

            if (output_type == 3)
            {
                check_pt = 1;
                if(line_ct2++ < 2)
                    continue;
            }

/* print lines making it thru the above filter
   to the (hopefully) appropriate destination file */

            fprintf(fp[output_type], "%s", buf);
        }
    }
/* done reading thru output. close our 4 files */

    for (i = 0; i <= 3; i++)
    {
        fclose(fp[i]);
    }
    fclose(output_fp);
    
    if (check_pt != 1)
    {
        printf("\nWARNING: The output file seems incomplete.\n");
        printf("  (can't find Channel Deposition data which should be\n");
        printf("   at the bottom of the file)\n\n");
        printf("Would you like to take a look at the output file to\n"); 
        if(G_yes("check for diagnostic (error) messages from ANSWERS?", 1))
        {
            sprintf(line, "%s/%s/%s/answers_output",
            G_location_path(), proj_mapset, data_dir);
            user_file(line);
        }
        complete[11] = 2;
        return(0);
    }

    printf("\n\n(NOTE: use the Miscellaneous Command Menu to look at the 'out_text'\n");
    printf("      file for more of the ANSWERS watershed summary)\n\n\n");
    hit_return();
    hydro_filter();
    map_sediment();
    return(0);
}

