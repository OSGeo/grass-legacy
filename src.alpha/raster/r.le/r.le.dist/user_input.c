	                 	/********************************/
			        /*   r.le.dist/user_input.c	*/
                	        /*                              */
				/*		2.1		*/
				/*				*/
	                        /*       07/10/94 version       */
				/*				*/
				/*      Programmer: Baker	*/
				/*      Univ. of Wyoming	*/
			        /********************************/

#include "r.le.dist.h" 

extern struct CHOICE *choice;  
extern int non_skip, NN;

void user_input(argc,argv) 
int argc;
char **argv;

{

   int i;

					/* setup the GRASS parsing routine
					   structures to be used to read
					   in the user's parameter choices */

   struct Flag *run;
   struct Flag *pat;
   struct Flag *trace;
   struct Flag *units;
   struct Option *name;
   struct Option *measure_code;
   struct Option *method_code;
   struct Option *sampling_method;
   struct Option *region; 
   struct Option *skip;
   struct Option *candidate_patches;
   struct Option *out;

					/* initialize the GRASS GIS system */

   G_gisinit(argv[0]);	

					/* use the GRASS parsing routines
					   to read in the user's parameter
					   choices */

   run = G_define_flag();
   run->key 		= 'b';
   run->description  	= "Run in background";

   pat = G_define_flag();
   pat->key          	= 'n';
   pat->description  	= "Output map 'num' with patch numbers";

   trace = G_define_flag();
   trace->key          	= 't';
   trace->description  	= "Use 4 neighbor tracing instead of 8 neighbor";

   units = G_define_flag();
   units->key          	= 'u';
   units->description  	= "Output maps 'units_x' with sampling units for each scale x ";

   name = G_define_option();
   name->key          	= "map";
   name->description  	= "Raster map to be analyzed";
   name->type         	= TYPE_STRING;
   name->gisprompt    	= "old,cell,raster";
   name->required     	= YES;

   sampling_method = G_define_option();
   sampling_method->answer	= "w";
   sampling_method->key     	= "sam";
   sampling_method->description = "Sampling method (choose only 1 method): \n\t\t w=whole map, u=units, m=moving window, r=regions";
   sampling_method->options  	= "w,u,m,r";
   sampling_method->type       	= TYPE_STRING;
   sampling_method->multiple	= NO;
   sampling_method->required   	= NO;

   region = G_define_option();
   region->key          = "reg";
   region->description  = "Name of regions map, only when sam = r; omit otherwise";
   region->type         = TYPE_STRING;
   region->gisprompt    = "old,cell,raster";
   region->required     = NO;

   skip = G_define_option();
   skip->answer		= "0";
   skip->key          	= "ski";
   skip->description  	= "Skip m boundary cells to speed up nearest neighbor search";
   skip->options  	= "0-10";
   skip->type         	= TYPE_INTEGER;
   skip->required     	= NO;

   candidate_patches = G_define_option();
   candidate_patches->answer		= "30";
   candidate_patches->key          	= "can";
   candidate_patches->description  	= "Use only 'can' candidate patches for faster nearest neighbor search";
   candidate_patches->options      	= "1-30";
   candidate_patches->type         	= TYPE_INTEGER;
   candidate_patches->required     	= NO;

   method_code = G_define_option();
   method_code->key     	= "di1";
   method_code->description  	= "Distance methods (Choose only 1 method):  \n\t    (CC=Center-Center, EE=Edge-Edge, CE=Center-Edge):\n\t\t m0 = each patch to all adjacent neighbors CC\n\t\t m1 = each patch to all adjacent neighbors CE\n\t\t m2 = each patch to nearest patch of same gp CC\n\t\t m3 = each patch to nearest patch of same gp CE\n\t\t m4 = each patch to nearest patch of same gp EE\n\t\t m5 = each patch to nearest patch of any diff. gp CC\n\t\t m6 = each patch to nearest patch of any diff. gp CE\n\t\t m7 = patches of 1 gp to nearest of specific gp CC\n\t\t m8 = patches of 1 gp to nearest of specific gp CE\n\t\t m9 = patches of 1 gp to nearest of specific gp EE";
   method_code->options      	= "m0,m1,m2,m3,m4,m5,m6,m7,m8,m9";
   method_code->multiple     	= YES;
   method_code->type         	= TYPE_STRING;
   method_code->required     	= NO;

   measure_code = G_define_option();
   measure_code->key     	= "di2";
   measure_code->description  	= "Distance measures:  \n\t\t n1 = mean dist.\t\t n2 = st. dev. dist.\n\t\t n3 = mean dist. by gp\t\t n4 = st. dev. dist. by gp\n\t\t n5 = no. of dist. by dist. class\n\t\t n6 = no. of dist. by dist. class by gp";
   measure_code->options  	= "n1,n2,n3,n4,n5,n6";
   measure_code->multiple 	= YES;
   measure_code->type         	= TYPE_STRING;
   measure_code->required     	= NO;

   out = G_define_option();
   out->key     		= "out";
   out->description 		= "Name of output file for individual patch measures, when sam=w,u,r;\n\t    if out=head, then column headings will be printed";
   out->type         		= TYPE_STRING;
   out->required     		= NO;
	

   if (G_parser(argc,argv)) {	
	exit(-1);
   }

   					/* record the user inputs for map,
					   sam, run, and out parameters */

   G_strcpy(choice->fn,name->answer);

   choice->wrum = sampling_method->answer[0];

   choice->fb = run->answer;

   if (out->answer && choice->wrum != 'm')
      G_strcpy(choice->out,out->answer);
   else if (out->answer && choice->wrum == 'm') {
         printf("\n");
         printf("   ***************************************************\n");
         printf("    You can use the out parameter only when sam=w,u,r \n");
         printf("   ***************************************************\n");
         exit(0);
   } 
   else
      G_strcpy(choice->out,"");

   non_skip = atoi(skip->answer);
   
   NN = atoi(candidate_patches->answer);

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

					/* check for multiple values for 
					   parameters that accept only 1 */

   if (sampling_method->answer)
      if(sampling_method->answers[1]) {
         printf("\n");
         printf("   **********************************************\n");
         printf("    You input multiple values for parameter sam, \n");
         printf("    but only one is allowed                      \n");
         printf("   **********************************************\n");
         exit(0);
      }

					/* if the pat flag -n is specified,
					   then set the choice->patchmap flag
					   to 1 */


   choice->patchmap = 0;
   if (!strcmp(sampling_method->answer,"w") && pat->answer)
      choice->patchmap = 1;
   else if (strcmp(sampling_method->answer,"w") && pat->answer) {
      printf("\n");
      printf("   **********************************************\n");
      printf("    You requested output of map 'num' with patch \n");
      printf("    numbers, by using flag -n, but this option   \n");
      printf("    is only available when sam=w                 \n");
      printf("   **********************************************\n");
      exit(0);
   }

					/* if the 4 neighbor tracing flag -t
					   is specified, then set the 
					   choice->trace flag to 1 */

   choice->trace = 1;
   if (trace->answer)
      choice->trace = 0;

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

					/* initialize the flag arrays in choice
					   data structure */
   
   for (i=0;i<6;i++)  choice->mm[i] = 0;
   for (i=0;i<10;i++) choice->mn[i] = 0;

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


   if (measure_code->answer && method_code->answer) {
      if(method_code->answer && method_code->answers[1]) {
         printf("\n");
         printf("   **********************************************\n");
         printf("    You input multiple values for parameter di1, \n");
         printf("    but only one is allowed                      \n");
         printf("   **********************************************\n");
         exit(1);
      }
      for (i=0; measure_code->answers[i] != NULL; i++) {
         if (!strcmp(measure_code->answers[i],"n1")) choice->mm[0]=1;
	 if (!strcmp(measure_code->answers[i],"n2")) choice->mm[1]=1;
         if (!strcmp(measure_code->answers[i],"n3")) choice->mm[2]=1;	
         if (!strcmp(measure_code->answers[i],"n4")) choice->mm[3]=1;	
	 if (!strcmp(measure_code->answers[i],"n5")) choice->mm[4]=1;
         if (!strcmp(measure_code->answers[i],"n6")) choice->mm[5]=1;
      }
           if (!strcmp(method_code->answer,"m0")) choice->mn[0] = 1;
      else if (!strcmp(method_code->answer,"m1")) choice->mn[1] = 1;	
      else if (!strcmp(method_code->answer,"m2")) choice->mn[2] = 1;	
      else if (!strcmp(method_code->answer,"m3")) choice->mn[3] = 1;	
      else if (!strcmp(method_code->answer,"m4")) choice->mn[4] = 1;	
      else if (!strcmp(method_code->answer,"m5")) choice->mn[5] = 1;	
      else if (!strcmp(method_code->answer,"m6")) choice->mn[6] = 1;	
      else if (!strcmp(method_code->answer,"m7")) choice->mn[7] = 1;	
      else if (!strcmp(method_code->answer,"m8")) choice->mn[8] = 1;	
      else if (!strcmp(method_code->answer,"m9")) choice->mn[9] = 1;
              
   }
   else {
      printf("\n");
      printf("   *************************************************\n");
      printf("    You requested distance measurement, but did not \n");
      printf("    input both parameter di1 and di2                \n");
      printf("   *************************************************\n");
      exit(1);
   }      
}
