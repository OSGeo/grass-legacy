#define GLOBAL
#include "global.h"

static char title[80];
static char *vinfo[]=
{
    title,
    "",
    "Please select the group/subgroup containing the signatures",
    "to be used in the classification",
NULL };

main(argc,argv) 

char *argv[];
{
    char *prompt;
    int i,tempi;

    G_gisinit (argv[0]);
    if (G_maskfd() >= 0)
    {
	printf ("\nWARNING: you have your mask set.\n");
	if (!G_yes("Do you want to continue? ", -1)) exit(0);
    }



    I_location_info (title, "AFFINITY CLASSIFICATION");

    t_mapset = G_ask_cell_old ("Enter map with training sites", t_map);
    if (t_mapset == NULL) exit(0);

    if(!I_vask_subgroup_old (vinfo, group, subgroup, 1, ""))
	exit(0);

    
/* classified layer */
/*
if (!G_ask_cell_new (
	"Please name the CLASSIFIED map layer to be generated",class_name))
	    exit(0);  */
G_ask_cell_new (
	"Please name the CLASSIFIED map layer to be generated",class_name);

/* reject layer - chi square tests */
G_ask_cell_new (
	 "Please name the REJECT THRESHOLD map layer to be generated", reject_name);


/* the type of each file to be classified   */    
/* ???? need to be refined later    ????? */
   
    open_group();  

 /*???? later may need close files  ???? */


   
    if (Ref.nfiles>MAX_BANDNUMBER) 
	{
		fprintf (stderr,"The number of all layers more than %d",MAX_BANDNUMBER);
		exit(1);
	}  
   
/*    printf("\n");
      nozero=G_yes("would you like to filter out zero data?",-1);
      printf("\n");

 printf("zero: %d",zero);
*/

    printf ("\nPlease input the TYPE of each file in this subgroup:");
    printf ("\n(1: quantitative, 2: quanlitative, 3: ranked) \n");
   
    for(i=0;i<Ref.nfiles;i++) {
       for(;;){
          printf("\nfile name: %s", Ref.file[i].name);
          scanf("   %d",&tempi);
          if (tempi>0 && tempi<4) break;
         }

       types[i]=tempi;

     }

printf("\n");

open_files();

fprintf (stderr, "\n\n%s ... ", G_program_name());

affinity();

exit(0);

}
