
#include "gis.h"

#include "r.le.null.h"

extern struct CHOICE *choice ;
extern struct CAT *cats ;

user_input(argc,argv)  
int argc ;
char **argv ;

{
   int i;

   struct Option *name;
   struct Option *ncats;
   struct Option *attr;
   struct Option *prob;


   name = G_define_option() ;
   name->key          = "map" ;
   name->description  = "Raster map, with neutral structure, to be created";
   name->type         = TYPE_STRING;
   name->gisprompt    = "new,cell,raster";
   name->required     = YES ;


   ncats = G_define_option() ;
   ncats->key 		= "num" ;
   ncats->description 	= "Number of attributes desired in the map (max = 24)";
   ncats->type 		= TYPE_INTEGER ;
   ncats->required 	= YES ;


   attr = G_define_option();
   attr->key 		= "att" ;
   attr->description 	= "Attribute i (an integer) of 'num' attributes";
   attr->type 		= TYPE_INTEGER ;
   attr->required	= YES ;
   attr->multiple	= YES ;

   prob = G_define_option();
   prob->key		= "pro" ;
   prob->description	= "Probability (as a % between 0-100) for attribute i of 'num' attributes";
   prob->type		= TYPE_INTEGER;
   prob->required	= YES ;
   prob->multiple	= YES ;


   if (G_parser(argc,argv))
      exit(-1) ;

   G_strcpy(choice->fn,name->answer) ;

   if (ncats->answer)
      choice->ncats = atoi(ncats->answer) ;

   for (i=0; i<25; i++) {
      cats->att[i] = 0;      
      cats->prob[i] = 0.0;
   }

   if (attr->answers) {
      for (i=0; attr->answers[i] != NULL; i++) {
         cats->att[i] = atoi(attr->answers[i]);
      }
   }

   if (prob->answers) {
      for (i=0; prob->answers[i] != NULL; i++) {
         cats->prob[i] = (double) atoi(prob->answers[i])/100;
      }
   }

}
