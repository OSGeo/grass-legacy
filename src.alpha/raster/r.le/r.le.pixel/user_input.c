				/********************************/
				/*   r.le.pixel/user_input.c	*/
                		/*                              */
				/*		2.1		*/
				/*				*/
                		/*       07/05/94 version       */
                		/*                              */
                        	/*      Programmer: Baker       */
                       	 	/*      Univ. of Wyoming        */
				/********************************/


#include "r.le.pixel.h"
								
extern struct CHOICE *choice;

void user_input(argc,argv)  
int argc;
char **argv;

{

   int i;

					/* setup the GRASS parsing routine
					   structures to be used to read 
					   in the user's parameter choices */

   struct Flag *run;
   struct Flag *units;
   struct Flag *zscore;
   struct Flag *edgemap;
   struct Option *name;
   struct Option *sampling_method;
   struct Option *region;
   struct Option *att;
   struct Option *diversity;
   struct Option *measure_code;
   struct Option *method_code;
   struct Option *juxtaposition;
   struct Option *edge;

					/* initialize the GRASS GIS system */

   G_gisinit(argv[0]);

					/* use the GRASS parsing routines
					   to read in the user's parameter
					   choices */

   run = G_define_flag();
   run->key 		= 'b';
   run->description  	= "Run in background";

   edgemap = G_define_flag();
   edgemap->key 	= 'e';
   edgemap->description = "Output map 'edge' of edges given a '1' in r.le.para/edge file";

   units = G_define_flag();
   units->key          	= 'u';
   units->description  	= "Output maps 'units_x' with sampling units for each scale x ";

   zscore = G_define_flag();
   zscore->key          = 'z';
   zscore->description  = "Output map 'zscores' with standardized scores";

   name = G_define_option();
   name->key          	= "map";
   name->description 	= "Raster map to be analyzed";
   name->type         	= TYPE_STRING;
   name->gisprompt    	= "old,cell,raster";
   name->required     	= YES;

   sampling_method = G_define_option();
   sampling_method->answer	 = "w";
   sampling_method->key          = "sam";
   sampling_method->description  = "Sampling method (choose only 1 method):   \n\t\t w=whole map, u=units, m=moving window, r=regions";
   sampling_method->type         = TYPE_STRING;
   sampling_method->multiple	 = NO;
   sampling_method->required     = NO;

   region = G_define_option();
   region->key          = "reg";
   region->description  = "Name of regions map, only when sam = r; omit otherwise";
   region->type         = TYPE_STRING;
   region->gisprompt    = "old,cell,raster";
   region->required     = NO;

   att			= G_define_option();
   att->key		= "att";
   att->description	= "Attribute measures:  \n\t\t b1 = mean pixel attribute\tb2 = st. dev. pixel attribute \n\t\t b3 = minimum pixel attribute\tb4 = maximum pixel attribute";
   att->options		= "b1,b2,b3,b4";
   att->type		= TYPE_STRING;
   att->multiple	= YES;
   att->required	= NO;

   diversity = G_define_option();
   diversity->key          	= "div";
   diversity->description  	= "Diversity measures:  \n\t\t d1 = richness\t\t\td2 = Shannon  \n\t\t d3 = dominance\t\t\td4 = inverse Simpson";
   diversity->options  		= "d1,d2,d3,d4";
   diversity->type         	= TYPE_STRING;
   diversity->multiple    	= YES;
   diversity->required     	= NO;

   method_code = G_define_option();
   method_code->key     	= "te1";
   method_code->description  	= "Texture method (choose only 1 method): \n\t\t m1 = 2N-H\tm2 = 2N-45\tm3 = 2N-V\n\t\t m4 = 2N-135\tm5 = 4N-HV\tm6 = 4N-DIAG\n\t\t m7 = 8N";
   method_code->options         = "m1,m2,m3,m4,m5,m6,m7";
   method_code->type         	= TYPE_STRING;
   method_code->multiple	= NO;
   method_code->required     	= NO;

   measure_code = G_define_option();
   measure_code->key     	= "te2";
   measure_code->description  	= "Texture measures (required if te1 was specified):  \n\t\t t1 = contagion\t\t\tt2 = ang. sec. mom.\n\t\t t3 = inv. diff. mom.\t\tt4 = entropy\n\t\t t5 = contrast";
   measure_code->options  	= "t1,t2,t3,t4,t5";
   measure_code->type         	= TYPE_STRING;
   measure_code->multiple	= YES;
   measure_code->required     	= NO;

   juxtaposition = G_define_option();
   juxtaposition->key          	= "jux";
   juxtaposition->description  	= "Juxtaposition measures (weight file in r.le.para needed):\n\t\t j1 = mean juxtaposition\n\t\t j2 = standard deviation of juxtaposition";
   juxtaposition->options      	= "j1,j2";
   juxtaposition->type         	= TYPE_STRING;
   juxtaposition->multiple	= YES;
   juxtaposition->required     	= NO;

   edge = G_define_option();
   edge->key          		= "edg";
   edge->description  		= "Edge measures:  \n\t\t e1 = sum of edges\n\t\t e2 = sum of edges by type (edge file in r.le.para needed)";
   edge->options      		= "e1,e2";
   edge->type         		= TYPE_STRING;
   edge->multiple		= YES;
   edge->required     		= NO;


   if (G_parser(argc,argv))
      exit(-1);

					/* record the user inputs for map,
					   sam, run, and out parameters */

   G_strcpy(choice->fn,name->answer);

   choice->wrum = sampling_method->answer[0];

   choice->fb = run->answer;

					/* check for unacceptable values for
					   input parameters */

   if (strcmp(sampling_method->answer,"w") && 
       strcmp(sampling_method->answer,"u") &&
       strcmp(sampling_method->answer,"m") && 
       strcmp(sampling_method->answer,"r")) {
         printf("\n");
         printf("   ***************************************************\n");
         printf("    You input an unacceptable value for parameter sam \n");
         printf("   ***************************************************\n");
         exit(0);
   }

                                        /* check for multiple values for te1 */

   if(method_code->answer)
      if(method_code->answers[1]) {
         printf("\n");
         printf("   **********************************************\n");
         printf("    You input multiple values for parameter te1, \n");
         printf("    but only one is allowed                      \n");
         printf("   **********************************************\n");
         exit(0);
      }


					/* if the -u flag is specified, then
					   set the choice->units flag to 1 */

   choice->units = 0;
   if (!strcmp(sampling_method->answer,"u") && units->answer)
      choice->units = 1;
   else if (strcmp(sampling_method->answer,"u") && units->answer) {
      printf("\n");
      printf("   ***************************************************\n");
      printf("    You requested output of map 'units' with sampling \n");
      printf("    units, by using flag -u, but this option is only  \n");
      printf("    available when sam=u                              \n");
      printf("   ***************************************************\n");
      exit(0);
   }

					 /* if sampling_method is by REGION
					    get region file name.  Check to see
					    that the name was input */

   if (!strcmp(sampling_method->answer,"r")) {
      if (region->answer)
         G_strcpy(choice->reg,region->answer);
      else {
      	 printf("\n");
      	 printf("   ***********************************************\n");
      	 printf("    You requested sampling by region, but did not \n");
	 printf("    input the name of the region using the reg=   \n");
      	 printf("    parameter                                     \n");    
      	 printf("   ***********************************************\n");
      	 exit(0);
      }
   }

   if (region->answer)
      if (strcmp(sampling_method->answer,"r")) {           
      	 printf("\n");
      	 printf("   ***********************************************\n");
      	 printf("    You requested sampling by region, by using    \n");
	 printf("    the reg= parameter, but did not input the     \n");
	 printf("    sam=r parameter                               \n");    
      	 printf("   ***********************************************\n");
      	 exit(0);
   }

         				/* initialize flag arrays in choice 
					   data structure. */

   for (i=0;i<5;i++) choice->att[i] = 0;
   for (i=0;i<5;i++) choice->div[i] = 0;
   for (i=0;i<6;i++) choice->te2[i] = 0;
   for (i=0;i<3;i++) choice->jux[i] = 0;
   for (i=0;i<3;i++) choice->edg[i] = 0;

					/* fill measure_code and method
					   code arrays */


   if (att->answer){
      choice->att[0]=1;
      for (i=0; att->answers[i] != NULL; i++) {
	      if (!strcmp(att->answers[i],"b1")) choice->att[1]=1;
	 else if (!strcmp(att->answers[i],"b2")) choice->att[2]=1;
	 else if (!strcmp(att->answers[i],"b3")) choice->att[3]=1;
	 else if (!strcmp(att->answers[i],"b4")) choice->att[4]=1;
      }
   }

   if (edgemap->answer && choice->wrum == 'w') {
      choice->edgemap = edgemap->answer;
      choice->edg[0] = 1;
      choice->edg[2] = 1;
   }
   else if (edgemap->answer && choice->wrum != 'w') {
      printf("\n");
      printf("   ****************************************************\n");
      printf("    An edge map (flag is -e) is not available unless   \n");
      printf("    sam=w                                              \n");
      printf("   ****************************************************\n");
      exit(0);
   }

   if (zscore->answer && choice->wrum == 'w') {
      choice->z = zscore->answer;
      choice->att[0] = 1;
      choice->att[1] = 1;
      choice->att[2] = 1;
   }
   else if (zscore->answer && choice->wrum != 'w') {
      printf("\n");
      printf("   ****************************************************\n");
      printf("    A zscores map (flag is -z) is not available unless \n");
      printf("    sam=w                                              \n");
      printf("   ****************************************************\n");
      exit(0);
   }

   if (diversity->answer){
      choice->div[0]=1;
      for (i=0; diversity->answers[i] != NULL; i++) {
	      if (!strcmp(diversity->answers[i],"d1")) choice->div[1]=1;
	 else if (!strcmp(diversity->answers[i],"d2")) choice->div[2]=1;
	 else if (!strcmp(diversity->answers[i],"d3")) choice->div[3]=1;
	 else if (!strcmp(diversity->answers[i],"d4")) choice->div[4]=1;
      }
   }

   choice->tex = 0;
   if (measure_code->answer || method_code->answer) { 
      if (measure_code->answer && method_code->answer) {
	 choice->te2[0]=1;
         for (i=0; measure_code->answers[i] != NULL; i++) {
                 if (!strcmp(measure_code->answers[i],"t1")) choice->te2[1]=1; 
            else if (!strcmp(measure_code->answers[i],"t2")) choice->te2[2]=1;
            else if (!strcmp(measure_code->answers[i],"t3")) choice->te2[3]=1;
            else if (!strcmp(measure_code->answers[i],"t4")) choice->te2[4]=1;
            else if (!strcmp(measure_code->answers[i],"t5")) choice->te2[5]=1;
         }
              if (!strcmp(method_code->answer,"m1")) choice->tex = 1;	
         else if (!strcmp(method_code->answer,"m2")) choice->tex = 2;	
         else if (!strcmp(method_code->answer,"m3")) choice->tex = 3;	
         else if (!strcmp(method_code->answer,"m4")) choice->tex = 4;	
         else if (!strcmp(method_code->answer,"m5")) choice->tex = 5;	
         else if (!strcmp(method_code->answer,"m6")) choice->tex = 6;	
         else if (!strcmp(method_code->answer,"m7")) choice->tex = 7;
      }
      else {			
         printf("\n");
         printf("   ************************************************\n");
         printf("    You requested texture measurement, but did not \n");
         printf("    input both parameter te1 and te2               \n");
         printf("   ************************************************\n");
         exit(0);
         }
   }

   if (juxtaposition->answer) {
      choice->jux[0]=1; 
      for (i=0; juxtaposition->answers[i] != NULL; i++) {
              if (!strcmp(juxtaposition->answers[i],"j1")) choice->jux[1]=1;
         else if (!strcmp(juxtaposition->answers[i],"j2")) choice->jux[2]=1;
      }
   }
 
   if (edge->answer) {
      choice->edg[0] = 1;
      for (i=0; edge->answers[i] != NULL; i++) {
              if (!strcmp(edge->answers[i],"e1")) choice->edg[1]=1;
         else if (!strcmp(edge->answers[i],"e2")) choice->edg[2]=1;
      }
   }

   if (!att->answer && !diversity->answer && !measure_code->answer &&
       !juxtaposition->answer && !edge->answer && !zscore->answer && 
       !edgemap->answer) {
      printf("\n");
      printf("   **************************************************\n");
      printf("    You did not select any measures to be calculated \n");
      printf("   **************************************************\n");
      exit(0);
   }
}


