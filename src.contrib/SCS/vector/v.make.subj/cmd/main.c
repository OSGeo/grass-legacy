/* %W% %G% */
/* main.c    1.0   4/01/91
*
*  ------Rev 4.+ arguements --------------------------------------
*    Input arguements:
*             v.make.subj map=  list of vector files to read
*                         subj= output SUBJ file to create
*
*/  
                                                                    
#include <stdio.h>
#include <ctype.h>
#include "gis.h"


main (argc, argv)
int argc;
char *argv[];
{
	register int i,k, ret, error;
	int old_cat_cnt=0, cat_cnt=0, recd, record, reccnt;
	int area_id, search, icode, cat_num;
	char path[1024],buffer[100], Obuffer[100];
	char area_name[100], cat_name[100], sav_name[100];
	char *in_name, *out_name, *mapset, *G_tempfile(), *tempname;
	char *nptr, *cptr, null='\0';
	char errmsg[200];
	FILE *Out, *In, *fopen (), *tmp_file;
	struct Option *map, *subj;
        struct Categories cats;

	setbuf (stdout, NULL);
	G_gisinit (argv[0]);

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
        if ((ret = access(path,0)) != -1)
           {            /* the category file exists, copy categ. to tmp */
           Out = fopen (path,"r");
	   tmp_file = fopen (tempname,"a");           
                        /*  get current category count */
           fgets (buffer, sizeof(buffer), Out);         
           sscanf(buffer,"%*s%d%*s",&old_cat_cnt);
                        /*  read past next four records */
           for (recd=0;recd<=3;++recd)                     
             {                       
             fgets (buffer, sizeof(buffer), Out);
             }        /*  copy remaining records to tmp */
           for (recd=0;;++recd)                              
             {                
             if (!fgets (buffer, sizeof(buffer), Out)) break;
             fputs (buffer,tmp_file);                              
             }                          
           fclose (Out);
           fclose (tmp_file);      
           tmp_file = fopen (tempname,"r");
           }
        else   
	   tmp_file = fopen (tempname,"a");

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
		getenv("GISDBASE"),getenv("LOCATION_NAME"),mapset,in_name);
	    fprintf (stdout, "\n    Reading category file %s\n", in_name);
            if ( (In = fopen(path, "r") ) == NULL )
		G_fatal_error("Reading input file.") ;

                     /* Cycle through file1 */
                        /*  read past first five records */
            for (recd=0;recd<=4;++recd)                     
               {                       
               fgets (buffer, sizeof(buffer), In);
               }       
            for (record=0;;++record)
               {                    /* select category records */
               if (!fgets (buffer, sizeof(buffer), In)) break;
               reccnt++;                                         
/*fprintf(stderr,"\tlength cat input = %d\n\t<%s>\n",strlen(buffer),buffer);*/
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
/*fprintf(stderr,"record %d  %d Bytes in !%s!\n",record,k,nptr); */

                          /* find input string in category file, 
			     assign category value to the
                             area_name based on category file record number*/
               search = 1;                                          
               icode = 0;  
               rewind (tmp_file);
               while ( search )   
                 {         
                 for (recd=0;;++recd)
                   {            /* category file search */
                   if (!fgets (buffer, sizeof(buffer), tmp_file)) break;
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
/*fprintf(stderr," comp leng= %d  cat |%s|  subjcat |%s|  subjrec %d\n",k,nptr,cptr,recd);
sleep (1);*/
	                        /* compare for match */
                   if (strcmp(nptr,cptr) == 0)
                      {       /* match, assigned already */
                      icode = cat_num;                            
                      break;     
                      }      
                   }         /* end of category search */
at_end:                                                            
                 if (icode == 0)
                   {      /* no match, add this name */
                   fclose (tmp_file);                   
                   tmp_file = fopen (tempname,"a");
                   icode = cat_num+1;                     
                   sprintf (Obuffer,"%d:%s\n\0",icode,sav_name);
                   fputs (Obuffer, tmp_file);                
                   fclose (tmp_file);         
                   if ((old_cat_cnt - cat_cnt) == 0)
                   fprintf(stderr,"\t   Adding new categ. :     ");    
                   cat_cnt++;
                   fprintf(stderr,"\b\b\b\b%4d",cat_cnt);
/*fprintf(stderr,"\n\t new code - cat (%d) !%s!, cnt %d\n",icode,nptr,cat_cnt);     
sleep(1);*/
                   tmp_file = fopen (tempname,"r");
                   break;
                   }
                 search = 0;
                 }             /* end while loop */
               }               /* end select category records */
             fclose(In);

	}

                 /* all done, good-bye */
        fprintf(stderr,"\n");
  fprintf(stderr,"\tSUBJect category processing finished\n\t  %d categorie(s) added\n",cat_cnt - old_cat_cnt);
                 /* make a final mapset master category file
                 put in first three records  */
        sprintf(path,"%s/%s/%s/SUBJ/%s",
		getenv("GISDBASE"),getenv("LOCATION_NAME"),mapset,out_name);
        Out = fopen (path,"w");
/*      rewind (Out); */
        sprintf (Obuffer,"#%5d categories\n\0",cat_cnt);
        fputs(Obuffer,Out);
        sprintf(Obuffer,"Title\n\0");
        fputs(Obuffer,Out);
        sprintf(Obuffer,"\n0.00 0.00 0.00 0.00\n0:  \n\0");
        fputs(Obuffer,Out);
        rewind (tmp_file);
                  /*   now append the tmp_file data  */
        for (recd=0;;++recd)         
          {                  
          if (!fgets (buffer, sizeof(buffer), tmp_file)) break;
          fputs (buffer,Out);                    
          }                             
        fclose (tmp_file);
        fclose (Out);

		  /* now read the subj file for sorting */
        G_suppress_warnings (1);
	G__read_cats ("SUBJ", out_name, G_mapset(), &cats, 1);
  	G_suppress_warnings (0); 
	sort_by_label (&cats);
		  /* have to put in "no data" category */
	cats.list[0].label = G_store("no data");
	G__write_cats ("SUBJ", out_name, &cats);
                  /* remove the tmp_file*/
        unlink(tmp_file); 
	exit (0);
}

sort_by_label(pcats)
struct Categories *pcats;
{
        int i, cmp();

	if (pcats->count > 1)
	    qsort (pcats->list, pcats->count, sizeof (struct Cat_List), cmp);

	 /* labels will be shifted, renumber */
        for (i=1; i<pcats->count; i++)
	              pcats->list[i].num = i;
}

cmp (a, b)
struct Cat_List *a, *b;
{
	if(strcmp(a->label , b->label) < 0) 
	    return -1;
	if(strcmp(a->label , b->label) > 0)  
	    return 1;
	return 0;
}
