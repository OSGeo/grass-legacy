				/********************************/
				/*	r.le.pixel/main.c	*/
				/*				*/
				/*		2.1		*/
				/*				*/
				/*	Version 07/05/94	*/
				/*				*/
				/*      Programmer: Baker	*/
				/*       Univ. of Wyoming	*/
				/********************************/


#include "r.le.pixel.h"

struct CHOICE *choice;


main(argc, argv)
int   argc;
char **argv;
{
  DIR           *dp;

  char map_name[50];
  char *mapset;
  int ch;

  char buf[100];   

					/* initialize the GRASS GIS system */

  G_gisinit(argv[0]); 

					/* allocate space for the choice
					   data structure */

  choice = (struct CHOICE *)G_calloc(1, sizeof(struct CHOICE));

					/* call user_input to read in the
					   parameters */

  user_input(argc,argv);

					/* display the parameter choices */

  printf("\nPARAMETER CHOICES:\n");
  printf("\tMAP:\t  %s\n", choice->fn);
  if (choice->wrum == 'r')
     printf("\tREGION:\t  %s\n", choice->reg);

  printf("\tSAMPLE:"); 
  if (choice->wrum == 'w')  printf("\t  whole map    ");
  if (choice->wrum == 'm')  printf("\t  moving window");
  if (choice->wrum == 'u')  printf("\t  units        ");
  if (choice->wrum == 'r')  printf("\t  regions      ");

  printf("\t\tRUN IN:"); 
  if (choice->fb)  
     printf("\tbackground\n");
  else 
     printf("\tforeground\n");

  if (choice->edgemap || choice->units || choice->z)
     printf("\tOUTPUT MAPS:\n");
  if (choice->edgemap)
     printf("\t\t  edge\n");
  if (choice->units)
     printf("\t\t  units_x\n");
  if (choice->z)
     printf("\t\t  zscores\n");

  if(choice->att[0]) {
     printf("\tATTRIBUTE MEASURES:\n");
     if (choice->att[1]) printf("\t\t  mean pixel attribute\n");
     if (choice->att[2]) printf("\t\t  st. dev. pixel attribute\n");
     if (choice->att[3]) printf("\t\t  minimum pixel attribute\n");
     if (choice->att[4]) printf("\t\t  maximum pixel attribute\n");
  }

  if(choice->div[0]) {
     printf("\tDIVERSITY MEASURES:\n");
     if (choice->div[1]) printf("\t\t  richness\n");
     if (choice->div[2]) printf("\t\t  Shannon\n");
     if (choice->div[3]) printf("\t\t  dominance\n");
     if (choice->div[4]) printf("\t\t  inverse Simpson\n");
  }

  if(choice->te2[0]) {
     printf("\tTEXTURE METHOD:\n");
          if (choice->tex == 1) printf("\t\t  2N-H\n");
     else if (choice->tex == 2) printf("\t\t  2N-45\n");
     else if (choice->tex == 3) printf("\t\t  2N-V\n");
     else if (choice->tex == 4) printf("\t\t  2N-135\n");
     else if (choice->tex == 5) printf("\t\t  4N-HV\n");
     else if (choice->tex == 6) printf("\t\t  4N-DIAG\n");
     else if (choice->tex == 7) printf("\t\t  8N\n");
     printf("\tTEXTURE MEASURES:\n");
     if (choice->te2[1]) printf("\t\t  contagion\n"); 
     if (choice->te2[2]) printf("\t\t  ang. sec. mom.\n"); 
     if (choice->te2[3]) printf("\t\t  inv. diff. mom.\n"); 
     if (choice->te2[4]) printf("\t\t  entropy\n"); 
     if (choice->te2[5]) printf("\t\t  contrast\n"); 
  }

  if(choice->jux[0]) {
     printf("\tJUXTAPOSITION MEASURES:\n");
     if (choice->jux[1]) printf("\t\t  mean juxtaposition\n");
     if (choice->jux[2]) printf("\t\t  standard deviation of juxtaposition\n");
  }

  if(choice->edg[0]) {
     printf("\tEDGE MEASURES:\n");
     if (choice->edg[1]) printf("\t\t  sum of edges\n");
     if (choice->edg[2]) printf("\t\t  sum of edges by type\n");
  }

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


  if(choice->fb) {
     texture_back();
  }
					/* if running in the foreground */

  else {
     texture_fore();
  }

  free(choice);
}


