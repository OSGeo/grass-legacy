/* update GRASS 5 9/99 Bill Hughes */
/* main.c    1.0   4/01/91
*
*  ------Rev 4.+ arguements --------------------------------------
*    Input arguements:
*             v.merge map=  list of vector files to read
*                       output= new vector composite
*                       subj= SUBJ file to use
*
*                flags:
*                       -m use RAM for the subj file categories
*                       -v verbose mode
*/  
                                                                    
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include "gis.h"
/*#define DEBUG 1*/

int main (int argc, char *argv[])
{
	register int i,k, ret, error;
	int cnt, recd, record, mem=0, verbose=0;
	int area_id, search, icode, cat_num;
	char path[1024],buffer[100], Obuffer[100];
	char area_name[100], cat_name[100], sav_name[100];
	char *in_name, *subj_file, *mapset, *G_tempfile(), *tempname;
	char *nptr, *cptr, null='\0';
	char errmsg[200];
        char code;
        double x, y;
	FILE *Out, *Subj, *cat, *att, *tmp_file;
	struct GModule *module;
	struct Option *map, *new, *subj;
	struct Flag *m_flag, *v_flag;
        struct Categories cats;

	setbuf (stdout, NULL);
	G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Merges vector map files.";

	map = G_define_option();
	map->key			= "map";
	map->type			= TYPE_STRING;
	map->required		= YES;
	map->multiple		= YES;
	map->gisprompt		= "old,dig,vector";
	map->description		= "vector map--source for composite";

	new = G_define_option();
	new->key			= "output";
	new->type			= TYPE_STRING;
	new->required		= YES;
	new->multiple		= NO;
	new->gisprompt		= "new, dig, vector";
	new->description		= "new vector composite";

	subj = G_define_option();
	subj->key			= "subj";
	subj->type			= TYPE_STRING;
	subj->required		= YES;
	subj->multiple		= NO;
	subj->description		= "SUBJ file";

        m_flag = G_define_flag();
        m_flag->key              = 'm';
        m_flag->description      = "Use memory for subject category";

        v_flag = G_define_flag();
        v_flag->key              = 'v';
        v_flag->description      = "Show some action";

	if (G_parser (argc, argv))
		exit(-1);

	if (v_flag->answer) verbose = 1;

	subj_file = subj->answer;
	if (m_flag->answer)
	   {
	   mem = 1;
             /* get categories from SUBJ file */
           G_suppress_warnings (1);
	   G__read_cats ("SUBJ", subj_file, G_mapset(), &cats, 1);
  	   G_suppress_warnings (0);
	   }
        else
	   { /* open category file for reading, if it exists*/
	   sprintf(path,"SUBJ/%s",subj_file);
           Subj = fopen (path,"r");
	   }
	     /* create a temp file for att updates */
        tempname = G_tempfile() ;

	fprintf (stdout, "\n");
	i = 0;
	while (map->answers[i])
	    {
	    in_name = map->answers[i++];
	    if ((mapset = G_find_vector2 (in_name, "")) == NULL)
	      {
	      sprintf (errmsg, "Could not find vector file <%s>\n", in_name);
	      G_fatal_error (errmsg);
	      }

	    fprintf (stdout, "\n Pre-processing map file %s   ", in_name);
	    if (!verbose) fprintf(stdout,"\n");

	              /* Open dig_cats file(i) */
            sprintf(path,"%s/%s/%s/dig_cats/%s",
		getenv("GISDBASE"),getenv("LOCATION_NAME"),mapset,in_name);
            if ( (cat = fopen(path, "r") ) == NULL )
		G_fatal_error("Reading category file.") ;

	              /* Open dig_att file(i) */
            sprintf(path,"%s/%s/%s/dig_att/%s",
		getenv("GISDBASE"),getenv("LOCATION_NAME"),mapset,in_name);
            if ( (att = fopen(path, "r") ) == NULL )
		G_fatal_error("Reading attribute file.") ;

	    tmp_file = fopen (tempname,"w");           
            cnt = -1;
                     /* Cycle through category file(i) */
            for (record=0;;++record)
               {                    /* select category records */
               if (!fgets (buffer, sizeof(buffer), cat)) break;
		     /* skip past header stuff first */
               if (record < 5) continue;
               cnt++;
               sscanf (buffer,"%d:%s",&area_id,area_name); 
	       sprintf(sav_name,"%s",area_name);
	       nptr = area_name;
		  /* look for field separating colons (SCS version) */
	       while (*nptr != '\0')
		 {
		 if (*nptr == '\072')
		    {
		    *nptr = '\0';
		    break;
		    }
		 nptr++;
		 }
 
	       nptr = area_name;
               k = strlen(area_name);

                          /* find input string in cats 
			     assign category value to the
                             area_name based on cats record number*/
               icode = 0;  
	       if (mem)
		 {         /* use cats list stored in RAM */
                 for (recd=1;recd<cats.num;recd++)
                   {            /* cats search */
                   sprintf(cat_name,"%s",cats.labels[recd]);
		   cptr = cat_name;
		       /* look for field separating colons (SCS version) */
	             while (*cptr != '\0')
		       {
		       if (*cptr == '\072')
		          {
		          *cptr = '\0';
		          break;
		          }
		       cptr++;
		       }
 
                   cptr = cat_name;                                 
#ifdef DEBUG
fprintf(stderr," comp leng= %d  cat |%s|  subjcat |%s|  subjrec %d\n",k,nptr,cptr,recd);
sleep (1);
#endif
	                        /* compare for match */
                   if (strcmp(nptr,cptr) == 0)
                      {       /* match, assigned already */
                      icode = recd;                            
                      break;     
                      }      
		   }   /* end of for loop */
		 }    /* end cats list search in RAM */
	       else
		 {         /* read cats list from the disk file */
                 rewind (Subj);
                 for (recd=0;;++recd)
                     {            /* category file search */
                     if (!fgets (buffer, sizeof(buffer), Subj)) break;
/*fprintf(stderr,"\tsubj recd %d length input = %d\n\t<%s>\n",recd,strlen(buffer),buffer); */
                     sscanf (buffer,"%d:%s",&cat_num,cat_name);
                     cptr = cat_name;                                 
		       /* look for field separating colons (SCS version) */
	             while (*cptr != '\0')
		       {
		       if (*cptr == '\072')
		          {
		          *cptr = '\0';
		          break;
		          }
		       cptr++;
		       }
 
                     cptr = cat_name;                                 
#ifdef DEBUG
fprintf(stderr," comp leng= %d  cat |%s|  subjcat |%s|  subjrec %d\n",k,nptr,cptr,recd);
sleep (1);
#endif
	                        /* compare for match */
                     if (strcmp(nptr,cptr) == 0)
                        {       /* match, assigned already */
                        icode = cat_num;
                        break;     
                        }      
                     }    /* end of for loop */
                 }    /* end of file search */

               if (icode == 0)
                   {      /* no match, got invalid somewhere */
                   sprintf(errmsg,"\n\t cat !%s!, not found for %s\n",
					    nptr,in_name);     
	           G_fatal_error (errmsg);
                   }

                     /* Cycle through attribute file(i) */
               rewind(att);
               search = 0;                                          
               for (record=0;;++record)
                 {                    /* select attribute records */
                 if (!fgets (buffer, sizeof(buffer), att)) break;
	         sscanf (buffer, " %1c %lf %lf %d", &code, &x, &y, &cat_num);
                 if (cat_num == area_id)
		    {
#ifdef DEBUG
fprintf(stderr,"\tmatch %d, %d  change to %d\n",cat_num,area_id,icode);
#endif
		    search++;
                           /* write this to temp file */
                    sprintf(Obuffer, "%c    %10.2f   %10.2f    %6d\n",
			      code, x,y,icode);
                    fputs(Obuffer,tmp_file);
		    }
	         }               /* end of attribute search */
#ifdef DEBUG
if (search != 0)
   fprintf(stderr,"\n\t%d attributes changed for code %d\n",search,icode);
sleep(1);
#endif
	       if (verbose)
		 {
                 if (cnt == 0 || cnt == 4) fprintf(stdout,"\b\174");
                 if (cnt == 1 || cnt == 5) fprintf(stdout,"\b\057");
                 if (cnt == 2 || cnt == 6) fprintf(stdout,"\b\055");
                 if (cnt == 3 || cnt == 7) fprintf(stdout,"\b\134");
                 if (cnt > 7) cnt = -1;
		 }
               }               /* end select category records */
             fclose(cat);
             fclose(att);
             fclose(tmp_file);

                /* now copy the tmp_file to the att file */
             sprintf(path,"%s/%s/%s/dig_att/%s",
		getenv("GISDBASE"),getenv("LOCATION_NAME"),mapset,in_name);
  	     sprintf( buffer, "cp %s %s", tempname,path);

             if (system( buffer) )
	        {
	        sprintf(errmsg,
		    "ERROR(v.merge):  Could not replace dig_att file: '%s'\n",
			in_name) ;
	        G_fatal_error (errmsg);
	        }
	}
	if (!mem) fclose(Subj);

                 /* all done, now create and issue the patch command */
        fprintf(stderr,"\n");
        sprintf(path,"v.patch input=");
	i = 0;
	while (map->answers[i])
	    {
	    in_name = map->answers[i++];
	    sprintf(buffer,"%s,",in_name);
	    strcat(path,buffer);
	    }
        sprintf(buffer," output=%s",new->answer);
	strcat(path,buffer);

        if (system(path) )
	   G_fatal_error ("ERROR(v.merge):  Could not patch map files\n") ;

                  /* copy the SUBJ file to patched dig_cats file */
        sprintf(buffer,"cp %s/%s/%s/SUBJ/%s %s/%s/%s/dig_cats/%s",
		getenv("GISDBASE"),getenv("LOCATION_NAME"),mapset,subj_file,
		getenv("GISDBASE"),getenv("LOCATION_NAME"),mapset,new->answer);
        if (system(buffer) )
	   G_fatal_error ("ERROR(v.merge):  Could not create dig_cats file for %s\n",
	      new->answer ) ;

                  /* remove the tmp_file*/
        unlink(tempname); 
	exit (0);
}
