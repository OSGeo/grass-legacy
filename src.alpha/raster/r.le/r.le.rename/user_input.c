				/********************************/
				/*   r.le.rename/user_input.c	*/
				/*				*/
				/*	Version 12/10/92	*/
				/*				*/
				/*  Programmers: Bucher, Baker	*/
				/*       Univ. of Wyoming	*/
				/********************************/

#include "gis.h"

#include "r.le.rename.h"

extern struct CHOICE *choice ;

user_input(argc,argv)  
int argc ;
char **argv ;

{


   struct Option *extension ;
   struct Option *old_files ;
   struct Option *new_files ;
   struct Flag *all ;

   int n, fcount=0 ;


   extension = G_define_option() ;
   extension->key          = "ext" ;
   extension->description  = "new extension with which to replace the .out extension \n\t     of files in r.le out directory";
   extension->type         = TYPE_STRING;
   extension->required     = NO ;

   old_files = G_define_option() ;
   old_files->key          = "old" ;
   old_files->description  = "old file name i in r.le.out directory to be changed";
   old_files->type         = TYPE_STRING;
   old_files->multiple     = YES ;
   old_files->required     = NO  ;

   new_files = G_define_option() ;
   new_files->key          = "new" ;
   new_files->description  = "new file name for old file name i in r.le.out directory";
   new_files->type         = TYPE_STRING;
   new_files->multiple     = YES ;
   new_files->required     = NO  ;


   all = G_define_flag() ;
   all->key = 'a' ;
   all->description="all files in r.le.out that have extension .out change their\n\t     extension to parameter ext; others not affected";


   if (G_parser(argc,argv))
      exit(-1) ;

   if (extension->answer) 
      strcpy(choice->extension,extension->answer) ;

   if (all->answer)
      choice->process_all = 1 ;
   else
      choice->process_all = 0 ;


   if (old_files->answer)
      for(n=0; old_files->answers[n] != NULL; n++) {
         G_strcpy(choice->old_fnames[n],old_files->answers[n]) ;
         fcount++ ;
      }

   if (new_files->answer)
      for(n=0; new_files->answers[n] != NULL; n++) {
         G_strcpy(choice->new_fnames[n],new_files->answers[n]) ;
      }


   choice->fcount = fcount ;

}
