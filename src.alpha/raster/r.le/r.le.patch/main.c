				/********************************/
				/*	r.le.patch/main.c	*/
				/*				*/
				/*		2.1		*/
				/*				*/
				/*	Version 06/15/94	*/
				/*				*/
				/*       Programmer: Baker	*/
				/*       Univ. of Wyoming	*/
				/********************************/


#include "r.le.patch.h"

struct CHOICE *choice;

main(argc, argv)
int   argc;
char **argv ;
{
  DIR           *dp;

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

  printf("\t\tRUN IN:"); 
  if(choice->fb)
     printf("\tbackground\n");
  else
     printf("\tforeground\n");

  printf("\tTRACING:");
  if (choice->trace)
     printf("  8 neighbor\n");
  else
     printf("  4 neighbor\n");

  if (choice->coremap || choice->patchmap || choice->units)
     printf("\tOUTPUT MAPS:\n");
  if (choice->coremap)
     printf("\t\t  interior\n");
  if (choice->patchmap)
     printf("\t\t  num\n");
  if (choice->units)
     printf("\t\t  units_x\n");

  if(choice->att[0]) 
     printf("\tATTRIBUTE MEASURES:\n");
  if(choice->att[1])  printf("\t\t  mean pixel attribute\n");
  if(choice->att[2])  printf("\t\t  st. dev. pixel attribute\n");
  if(choice->att[3])  printf("\t\t  mean patch attribute\n");
  if(choice->att[4])  printf("\t\t  st. dev. patch attribute\n");
  if(choice->att[5])  printf("\t\t  cover by gp\n");
  if(choice->att[6])  printf("\t\t  density by gp\n");
  if(choice->att[7])  printf("\t\t  total density\n");

  if(choice->size[0]) 
        printf("\tSIZE MEASURES:\n");
  if(choice->size[1]) printf("\t\t  mean patch size\n");
  if(choice->size[2]) printf("\t\t  st. dev. patch size\n");
  if(choice->size[3]) printf("\t\t  mean patch size by gp\n");
  if(choice->size[4]) printf("\t\t  st. dev. patch size by gp\n");
  if(choice->size[5]) printf("\t\t  no. by size class\n");
  if(choice->size[6]) printf("\t\t  no. by size class by gp\n");

  if(choice->core[0])
	printf("\tCORE MEASURES:\n");
  if(choice->core[1]) printf("\t\t  mean core size\n");
  if(choice->core[2]) printf("\t\t  st. dev. core size\n");
  if(choice->core[3]) printf("\t\t  mean edge size\n");
  if(choice->core[4]) printf("\t\t  st. dev. edge size\n");
  if(choice->core[5]) printf("\t\t  mean core size by gp\n");  
  if(choice->core[6]) printf("\t\t  st. dev. core size by gp\n");
  if(choice->core[7]) printf("\t\t  mean edge size by gp \n");
  if(choice->core[8]) printf("\t\t  st. dev. edge size by gp\n");
  if(choice->core[9]) printf("\t\t  no. by size class \n");
  if(choice->core[10]) printf("\t\t  no. by size class by gp\n");

  if(choice->shape[0])
	printf("\tSHAPE MEASURES:\n");
  if(choice->shape[1]) printf("\t\t  mean patch shape\n");
  if(choice->shape[2]) printf("\t\t  st. dev. patch shape\n");
  if(choice->shape[3]) printf("\t\t  mean patch shape by gp\n");
  if(choice->shape[4]) printf("\t\t  st. dev. patch shape by gp\n");
  if(choice->shape[5]) printf("\t\t  no. by shape class\n");
  if(choice->shape[6]) printf("\t\t  no. by shape class by gp\n");

  if(choice->fract)  
     printf("\tFRACTAL MEASURES:\n\t\t  perimeter-area fractal dimension\n");

  if(choice->perim[0])
	printf("\tPERIMETER MEASURES:\n");
  if(choice->perim[1]) printf("\t\t  sum of perims\n");
  if(choice->perim[2]) printf("\t\t  mean perim.\n");
  if(choice->perim[3]) printf("\t\t  st. dev. perim.\n");
  if(choice->perim[4]) printf("\t\t  sum of perims. by gp\n");
  if(choice->perim[5]) printf("\t\t  mean perim. by gp\n");
  if(choice->perim[6]) printf("\t\t  st. dev. perim. by gp\n");

  					/* if not moving window, setup the
 					   r.le.out subdirectory */

  if(choice->wrum != 'm' && !(dp = opendir("r.le.out"))) { 
     G_system("mkdir r.le.out");
  }
  else {
     if (dp = opendir("r.le.out"))
        closedir(dp);
  }

					/* if running in the background */

  if(choice->fb)
     patch_back();

					/* if running in the foreground */

  else 
     patch_fore();

  free(choice);
}
