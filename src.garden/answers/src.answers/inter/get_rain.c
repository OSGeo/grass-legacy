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

   function: get_rain
   called by menu

   this module serves as an interface to register the user's desires
   for the set up of rain gauge data. if one gauge is to be used,
   there is little to do but call the next module (mk_rain). if multiple
   gauges are to be used, (answers allows up to 4), we need to know
   what cell layer dipicts the coverage of each gauge. in this case,
   after getting the name of the layer from the user, we check to make
   sure that it has no more than 4 cats, and use the number of cats to
   indicate the number of gauges. we then call the next module. if
   this step is run again after formerly being completed, we check to
   see whether the user wishes to start all over, or perhaps simply 
   modify the rain fall event.
*/
#include "answers.h"

char *rain_intro[] =
{
"     ANSWERS on GRASS Rainfall Data Utility",
"",
"This utility is designed to organize data used to describe the",
"precipitation event to be simulated. ANSWERS permits up to four rain",
"gauges to be used, each of which will require a table of rainfall",
"data (time in minutes and rainfall intensity in millimeters per hour).",
"Data from at least one rain gauge are required. If more than one gauge",
"is used, you will need to prepare a raster map of the watershed",
"area to indicate which cells are to represented by a given gauge's",
"data.",
"",
"Since you may wish to simulate several rainfall events, you ",
"will be prompted for a rainfall event name. The data tables entered",
"will be stored in the ANSWERS database under the event name.",
"",
"",
0
};
char *gauge_info[] =
{
"Format of Raingauge Data",
"",
"Rain gauge data for ANSWERS consists two columns of numbers.",
"The first is Time (in minutes) and the second is Rainfall Intensity",
"(in mm/hour). (Note: decimal values will be rounded up when input).",
"To input raingauge data to the Project Manager, a file must first",
"be prepared with rain gauge data.  If multiple gauges are to be used,",
"one input file is still used, data for each gauge are separated by",
"'-1' flag in the Time column.",
"",
"   example input files:",
"",
"     one gauge            two gauges",
"     _________            __________",
"       0   0               0  0",
"      10   3              11  3     data for gauge 1",
"      20  10              25  7",
"      35  22              -1       <----- delimiter",
"      55   9               0  0",
"      67   4              15  6     data for gauge 2",
"     100   0              10  4",
"",
0
};

get_rain()
{

    int         num_gauges;
    int 	i;
    char        line[200];

  /* Print module's opening message */

    G_clear_screen();
    for (i = 0; rain_intro[i]; i++)
	printf ("%s\n", rain_intro[i]);
    hit_return();
    
    G_clear_screen();
    for (i = 0; gauge_info[i]; i++)
	printf ("%s\n", gauge_info[i]);
    hit_return();
    G_clear_screen();
    
  /* if this module has been run before, give chance to use previous */

    if (complete[5] > 0)
        {
        sprintf(line, "Do you wish to use data from a previously entered event?");

        if(G_yes(line, 1))  
        {
            mk_rain(0);
            return(0);
        }
     }
    G_clear_screen();
    printf("\n<Creating new rainfall event>\n\n");

        sprintf(line, "Do you wish to use more than one rain gauge?");
        if(G_yes(line, 0)) { 
            printf("\n<Using data from more than one rain gauge.>\n");
            num_gauges = 0;
        }
        else{
            printf("\n<Using data from one rain gauge.>\n");
            num_gauges = 1;
            strcpy(rain_layer, "none");
        }


  /* if user wants to use more than one gauge ask for map */

    if (num_gauges == 0)
    {
        printf("\nEnter the name of the raster map which depicts rain gauge areas.\n");
        rain_mapset = G_ask_cell_old ("",rain_layer);
        if(!rain_mapset) 
	    return(0);
        if (mk_cat_tbl(1, rain_layer, rain_mapset) == -1)
        {
            return(0);
        }
        num_gauges = cat_tbl[0].cat;
        printf("\nNumber of categories found: %d\n\n", num_gauges);

/* error check cats */

    if(num_gauges < 2)
    {
        croak(1, "Not enough categories. Two is minimum.");
    }
    if(num_gauges > 4)
    {
        croak(1, "Too many categories. Four is maximum.");
    }
    for(i = 1; i <= num_gauges; i++)
    {
        if(cat_tbl[i].cat != i)
        {
            printf("ERROR: Category values in <%s> do not meet expectations\n", 
            rain_layer);
            printf("expecting category %d, but found %d\n", i, cat_tbl[i].cat);
            croak(1, "category values should be consecutive, starting with 1");
        }
    }
    }

    mk_rain(num_gauges);
    return (0);
}
