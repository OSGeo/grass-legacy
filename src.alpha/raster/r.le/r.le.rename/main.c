				/********************************/
				/*	r.le.rename/main.c	*/
				/*				*/
				/*	Version 10/10/92	*/
				/*				*/
				/*  Programmers: Bucher, Baker	*/
				/*       Univ. of Wyoming	*/
				/********************************/
#include "stdio.h"
#include "r.le.rename.h"

struct CHOICE *choice ;




				/* MAIN PROGRAM */

main(argc, argv)
int   argc;
char *argv[];
{
  struct dirent *dp;
  DIR  *dirp; 
  char  dir_path[80], file_name[80], extension[80] ;
  char  c, name[30], pathold[80], path[80], cmd[200], suffix[20];
  int   n, i, index ;


  choice = (struct CHOICE *) malloc(sizeof(struct CHOICE)) ;

  user_input(argc,argv) ;


				/* check for r.le.out directory */

  if(!(dirp = opendir("r.le.out"))){
	perror("Can't open directory \"r.le.out\", exit.");
	exit(-1);
  }

  for(dp = readdir(dirp); dp != NULL; dp = readdir(dirp)){

				/* if all the files are to have their
				   names changed */

    if(choice->process_all){
	if(dp->d_name[0] != '.' ){
                get_file_name_without_dot(file_name,extension,dp->d_name) ;
                if ( !strcmp(extension,"out") ) {
                   sprintf(dir_path,"r.le.out/") ;
		   sprintf(pathold, "r.le.out/%s", dp->d_name);
		   sprintf(path, "%s%s.%s", dir_path, file_name,
		      choice->extension);
		   sprintf(cmd, "mv %s %s", pathold, path);
		   puts(cmd);
		   system(cmd);
                }
	}

    } 

				/* if only some files are to have their
				   names changed */

    else if(!choice->process_all){
	if(dp->d_name[0] != '.' ){
		index = index_of_new_fname(dp->d_name) ;
                if (index >= 0) {
		    sprintf(path, "r.le.out/%s", choice->new_fnames[index] );
		    sprintf(pathold, "r.le.out/%s", dp->d_name);
		    sprintf(cmd, "        mv %s %s", pathold, path);
		    system(cmd);
                }
        }
     }
  }

  closedir(dirp);
  free(choice) ;
}







				/* GET THE NAME OF A FILE IN R.LE.OUT &
				   OMIT THE DOT IN THE NAME */

get_file_name_without_dot(file_name,extension,d_name) 
char *file_name, *extension, *d_name ;
{

   int i=0, j=0 ;

   while (d_name[i] != '\0' && d_name[i] != '.') {
      file_name[i] = d_name[i] ;
      i++ ;
   }

   if (d_name[i] == '.') {
      file_name[i] = '\0' ;
      i++ ;
      while(d_name[i] != '\0') {
         extension[j] = d_name[i] ;
         i++ ;
         j++ ;
      }
      extension[j] = '\0' ;
   }
   else 
      strcpy(extension,"\0") ;
  
   return ;

}




				/* IF THE FILE TO BE RENAMED IS D_NAME */

index_of_new_fname(d_name) 
char *d_name ;
{

   int i=0 ;

  while (i<choice->fcount) {
    if ( ! strcmp(choice->old_fnames[i],d_name) ) 
       return (i) ;
    i++ ;
  }
  return(-1) ;
}
