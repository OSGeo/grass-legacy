/* 
 * $Id$
* main.c    1.0   4/01/91
*
*  ------Rev 4.+ arguements --------------------------------------
*    Input arguements:
*             v.make.subj map=  list of vector files to read
*                         subj= output SUBJ file to create
*
*/  
                                                                    
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include "gis.h"

#define BUFSZ 100

int sort_by_label(struct Categories *);
int cmp(const void *, const void *);

int main (int argc, char *argv[])
{
	int i,cnt;
	int old_cat_cnt=0, cat_cnt=0;
	int area_id, icode, cat_num=0;
	char path[1024],buffer[BUFSZ], Obuffer[BUFSZ];
	char area_name[BUFSZ], cat_name[BUFSZ];
	char *in_name, *out_name, *mapset, *tempname;
	char *nptr, *cptr;
	char errmsg[200];
	FILE *Out, *In, *tmp_file;
	struct Option *map, *subj;
        struct Categories cats;
        struct GModule *module;

	setbuf (stdout, NULL);
	G_gisinit (argv[0]);
	
	/* Set description */
	module              = G_define_module();
	module->description = ""\
	"Create a 'subject' file from all category labels found in a set of listed vector maps.";
	
	map = G_define_option();
	map->key			= "map";
	map->type			= TYPE_STRING;
	map->required		= YES;
	map->multiple		= YES;
	map->gisprompt		= "old,dig,vector";
	map->description		= "vector map--source for composite";

	subj = G_define_option();
	subj->key			= "subj";
	subj->type			= TYPE_STRING;
	subj->required		= YES;
	subj->multiple		= NO;
	subj->gisprompt		= "new";
	subj->description		= "new SUBJ file";

	if (G_parser (argc, argv))
		exit(-1);

             /* create SUBJ directory, if not existing */
        G__make_mapset_element("SUBJ") ;
	out_name = subj->answer;

             /* open category file for reading, if it exists*/
	sprintf(path,"SUBJ/%s",out_name);
        tempname = G_tempfile() ;
        if ( access(path,0) != -1)
        {
            /* the category file exists, copy data to tmp */
           Out = fopen (path,"r");
	   tmp_file = fopen (tempname,"w+");           

            /*  get current category count */
           fgets (buffer, sizeof(buffer), Out);         
           sscanf(buffer,"%*s%d%*s",&old_cat_cnt);

            /*  read past next four records */
           for (cnt=0;cnt<=3;++cnt)                     
               fgets (buffer, sizeof(buffer), Out);

            /*  copy remaining records to temp */
           while(fgets (buffer, sizeof(buffer), Out))
               fputs (buffer,tmp_file);                              

           fclose (Out);
        }
        else   
            /* open the temp file */
	   tmp_file = fopen (tempname,"w+");

        cat_cnt = old_cat_cnt;

	fprintf (stdout, "\n");
	i = 0;
	while (map->answers[i])
	{
	    in_name = map->answers[i++];
	    if ((mapset = G_find_vector2 (in_name, "")) == NULL)
	    {
	      sprintf (errmsg, "Could not find category file <%s>\n", in_name);
	      G_fatal_error (errmsg);
	    }

	     /* Open dig_cats file */
            sprintf(path,"%s/%s/%s/dig_cats/%s",
		G_gisdbase(),G_location(),mapset,in_name);
	    fprintf (stdout, "\nReading category file %s\n", in_name);
            if ( (In = fopen(path, "r") ) == NULL )
		G_fatal_error("Reading input file.") ;

             /*  read past first five records */
            for (cnt=0;cnt<=4;++cnt)                     
            {                       
               fgets (buffer, sizeof(buffer), In);
            }       

             /* compare each category record to those already found */
            while(fgets (buffer, sizeof(buffer), In))
            {
               for(cnt=0,nptr=buffer;cnt<BUFSZ;cnt++,nptr++)
               {
                  if(*nptr!='\n')continue;
                  *nptr='\0';
                  break;
               }
               if(sscanf (buffer,"%d:%s",&area_id,area_name)!=2)
               {
                  sprintf(errmsg,"Error scanning data in %s",map->answers[i-1]);
                  G_warning(errmsg);
                  continue;
               }

               nptr=buffer;
               while (*nptr != ':')nptr++;
               strncpy(area_name,++nptr,BUFSZ);
 

                          /* find input string in category file, 
			     assign category value to the
                             area_name based on category file record number*/
               icode = 0;  
               rewind (tmp_file);
               while(fgets (buffer, sizeof(buffer), tmp_file))
               {
                 for(cnt=0,cptr=buffer;cnt<BUFSZ;cnt++,cptr++)
                 {
                    if(*cptr!='\n')continue;
                    *cptr='\0';
                    break;
                 }
                 if(sscanf (buffer,"%d:%s",&cat_num,cat_name)!=2)
                 {
                   fclose(tmp_file);
                   unlink(tempname);
                   G_fatal_error("Error scanning tmp file");
                 }

                 cptr=buffer;
                 while (*cptr != ':')cptr++;
                 strncpy(cat_name,++cptr,BUFSZ);

                 if (strcmp(area_name,cat_name) == 0)
                 {       /* match, assigned already */
                    icode = cat_num;                            
                    break;    
                 }      
               }         /* end for loop through tmp file */

               if (icode == 0)
               {      /* no match, add this name */
                 if(fseek(tmp_file,0,SEEK_END))
                 {
                    fclose(tmp_file);
                    unlink(tempname);
                    G_fatal_error("Error seeking end of tmp file");
                 }
                 icode = cat_num+1;                     
                 sprintf (Obuffer,"%d:%s\n",icode,area_name);
                 fputs (Obuffer, tmp_file);                
                 cat_cnt+=1;
               }
            }           /* end loop through the cat file */
            fclose(In);
            cnt=cat_cnt-old_cat_cnt;
            if(cnt>0)fprintf(stderr,"\tAdded %d new categories\n",cnt);
            old_cat_cnt=cat_cnt;

	}  /* repeat if necessary with a new category file */


                 /* all done, good-bye */
        fprintf(stderr,"\n");
        fprintf(stderr,"\tAdded a total of %d categories\n",cat_cnt);

                 /* make a final mapset master category file */
        sprintf(path,"%s/%s/%s/SUBJ/%s",
		G_gisdbase(),G_location(),G_mapset(),out_name);
        Out = fopen (path,"w");

                 /* put in first three records  */
        sprintf (Obuffer,"#%5d categories\n",cat_cnt);
        fputs(Obuffer,Out);
        sprintf(Obuffer,"Title\n");
        fputs(Obuffer,Out);
        sprintf(Obuffer,"\n0.00 0.00 0.00 0.00\n0:  \n");
        fputs(Obuffer,Out);

                  /*   now append the tmp_file data  */
        rewind (tmp_file);
        while(fgets (buffer, sizeof(buffer), tmp_file))
            fputs (buffer,Out);                    
        fclose (Out);

		  /* now read the subj file for sorting */
        G_suppress_warnings (1);
	G__read_cats ("SUBJ", out_name, G_mapset(), &cats, 1);
	sort_by_label (&cats); 
		  /* have to put in "no data" category */
	cats.labels[0] = G_store("no data");
	G__write_cats ("SUBJ", out_name, &cats); 
  	G_suppress_warnings (0); 
                  /* remove the tmp_file*/
        unlink(tempname); 
	exit (0);
}

int sort_by_label( struct Categories *pcats)
{
	if (pcats->num > 1)
	    qsort (pcats->labels, pcats->num, sizeof (char *), cmp);

	 /* labels will be shifted, renumber */
        /* for (i=1; i<pcats->count; i++)  Not needed after Categories change.
	              pcats->list[i].num = i;*/

	return 0;
}

int cmp (const void *a, const void *b)
{
	return strcmp(*(const char **) a, *(const char **) b);
}

