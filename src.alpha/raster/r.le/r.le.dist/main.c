                 		/********************************/
		                /* 	r.le.dist/main.c        */
                                /*                              */
				/*		2.1		*/
				/*				*/
                                /*      07/10/94 version        */
                                /*                              */
				/*      Programmer: Baker	*/
				/*	Univ. of Wyoming	*/
		                /********************************/


#include "r.le.dist.h"

struct CHOICE *choice;
int   	      non_skip, NN;

main(argc, argv)
int   argc;
char **argv;
{
  DIR           *dp;

  int i;

				/* initialize the GRASS GIS system */

  G_gisinit(argv[0]); 

				/* allocate space for the choice
				   data structure */

  choice = (struct CHOICE *)G_calloc(1, sizeof(struct CHOICE));

				/* call user_input to read in the 
				   parameters */

  user_input(argc,argv) ;
  				/* display the parameter choices */

  printf("\nPARAMETER CHOICES:\n");
  printf("\tMAP:\t  %s\n", choice->fn);
  if (choice->wrum == 'r')
     printf("\tREGION:\t  %s\n", choice->reg);

  printf("\tSAMPLE:"); 
  if(choice->wrum == 'w')  printf("\t  whole map    ");
  if(choice->wrum == 'm')  printf("\t  moving window");
  if(choice->wrum == 'u')  printf("\t  units        ");
  if(choice->wrum == 'r')  printf("\t  regions      ");

  printf("\tRUN IN:"); 
  if(choice->fb)
     printf("\tbackground\n");
  else
     printf("\tforeground\n");

  printf("\tTRACING:");
  if (choice->trace)
     printf("  8 neighbor\n");
  else
     printf("  4 neighbor\n");

  if (choice->patchmap || choice->units)
     printf("\tOUTPUT MAPS:\n");
  if (choice->patchmap)
     printf("\t\t  num\n");
  if (choice->units)
     printf("\t\t  units_x\n");

  printf("\tSKIP:\t  %d cells\n", ((non_skip >= 0) ? non_skip : 0));

  printf("\tCANDIDATES:\n");
  printf("\t\t  %d patches\n", NN);

  printf("\tMEASURES:\n");
  if(choice->mm[0])  printf("\t\t  mean dist.\n");
  if(choice->mm[1])  printf("\t\t  st. dev. dist.\n");
  if(choice->mm[2])  printf("\t\t  mean dist. by gp\n");
  if(choice->mm[3])  printf("\t\t  st. dev. dist. by gp\n");
  if(choice->mm[4])  printf("\t\t  no. of dist. by dist. class\n");
  if(choice->mm[5])  printf("\t\t  no. of dist. by dist. class by gp\n");

  printf("\tMETHOD:");
  if(choice->mn[0])  printf("\t  each patch to all adjacent neighbors CC\n");
  if(choice->mn[1])  printf("\t  each patch to all adjacent neighbors CE\n");
  if(choice->mn[2])  printf("\t  each patch to nearest patch of same gp CC\n");
  if(choice->mn[3])  printf("\t  each patch to nearest patch of same gp CE\n");
  if(choice->mn[4])  printf("\t  each patch to nearest patch of same gp EE\n");
  if(choice->mn[5])  printf("\t  each patch to nearest patch of diff. gp CC\n");
  if(choice->mn[6])  printf("\t  each patch to nearest patch of diff. gp CE\n");
  if(choice->mn[7])  printf("\t  patches of 1 gp to nearest of specific gp CC\n");
  if(choice->mn[8])  printf("\t  patches of 1 gp to nearest of specific gp CE\n");
  if(choice->mn[9])  printf("\t  patches of 1 gp to nearest of specific gp EE\n");     

 					/* if not moving window, setup the
 					   r.le.out subdirectory */

  if(choice->wrum != 'm' && !(dp = opendir("r.le.out")))  {    
     G_system("mkdir r.le.out");
  }
  else {
     if (dp = opendir("r.le.out")) 
        closedir(dp);
  }

					/* if running in the background */

  if(choice->fb)
     dist_back();

					/* if running in the foreground */

  else 
     dist_fore();

  free(choice);
}


