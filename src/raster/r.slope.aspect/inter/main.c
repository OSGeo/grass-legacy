#include "gis.h"

char *intro[] =
{
"SLOPE and ASPECT",
"",
"This program produces aspect and slope maps from elevation data.",
"The elevation layer must be true elevation values, not rescaled.",
"The aspect map will indicate the direction of the slopes in 360",
"directions.  The slope map will have slopes in degrees or in percent.",
"Please enter the elevation file, and the names for the resulting",
"aspect and slope maps.",
0
};

main (argc,argv) char *argv[];
{
    char elevation[256], *mapset;
    char aspect[256];
    char slope[256];
    char command[1024];
    int i;
    int zero_is_data;
    int units;
    char buf[25];
    char temp_buf[25];
    double zfactor, min_slp;


    G_gisinit (argv[0]);

    for (i = 0; intro[i]; i++)
	printf ("%s\n", intro[i]);
    
    mapset = G_ask_cell_old ("enter elevation file", elevation);
    if (!mapset) exit(0);

    G_set_ask_return_msg ("if you don't want this product");
    if (!G_ask_cell_new("enter aspect file", aspect)) *aspect = 0;
    G_set_ask_return_msg ("if you don't want this product");
    if (!G_ask_cell_new("enter slope file", slope)) *slope = 0;
    if (*aspect == 0 && *slope == 0)
    {
	printf ("no elevation products requested\n");
	exit(0);
    }
    if (G_yes("Do zero values in the elevation map represent true elevations? ", -1))
	zero_is_data = 1;
    else
	zero_is_data = 0;

/* run the command-line version */
    sprintf (command, "r.slope.aspect elevation='%s'", 
	G_fully_qualified_name(elevation, mapset));
    if (*aspect)
    {
	strcat (command, " aspect=");
	strcat (command, aspect);
    }
    if (*slope)
    {
	strcat (command, " slope=");
	strcat (command, slope);
    }
    if(*slope!=0) 
    {
        if(G_yes("Should the slope be reported in percent instead of degrees?", -1))
  	    strcat(command, " format=percent");
    }

    if (zero_is_data)
	strcat (command, " -z");
    
    while(1)
    {
        printf("\nenter  1 if elevation units are meters\n       2 if elevation units are feet\n       3 for other elevation units.\n>");
        while(!G_gets(temp_buf));
        if(sscanf(temp_buf, "%d", &units)!=1) continue;
        if ((units==1)||(units==2)||(units==3)) break;
        printf("\n You can only enter 1, 2 or 3\n");
    }
    if(units==2)
       zfactor = .3048000;
    else 
    {
           if(units==1)
                  zfactor = 1.0;
           else
           {
	      while(1)
	      {
                 printf("\nEnter the factor for converting elevation units to meters >");
		 while(!G_gets(temp_buf));
		 if(sscanf(temp_buf, "%lf", &zfactor)!=1) continue;
	         if (zfactor>0) break;
	         printf("\n zfactor must be positive real number\n");
              }
           }
    }	    
    sprintf(buf, " zfactor=%lf", zfactor);
    strcat (command, buf); 

    min_slp = 0.;
    printf("\nWARNING: The computations of aspect for the points with very\n");
    printf("small slope are very far from being exact \n");
    if(G_yes("Do you want to specify a minimum value of slope for which aspect is computed?",-1))
    {
       while(1)
       {
           printf("\nEnter the minimum value for slope (in percent):");
           while(!G_gets(temp_buf));
           if(sscanf(temp_buf, "%lf", &min_slp)!=1) continue;
           if (min_slp>=0)break;
           printf("\n min_slp must be non-negative real number\n");
       }
    }
    sprintf(buf, " min_slp=%lf", min_slp);
    strcat (command, buf); 

    exit (system(command));
}
