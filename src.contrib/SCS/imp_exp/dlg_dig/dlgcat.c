/* @(#)main.c	1.1   03/7/91  GRASS4.0
/* @(#)main.c	1.0   08/1/87 
* Created by:  R.Glenn, SCS
*             program reads in ascii flat file containing:
*                DLG area ID      Area Name
*   format:         (int)(space)(name[])
*             compares the name to a category file (existing master)
*             and assigns the same category number. If the category
*             file doesn't exist, program makes one; assigning category
*             values by name order. Program then creates a new attribute
*             file of:
*                DLG area ID      Area Name    GRASS code
*   format:           (int)(space)(name[])(tab)(int)
*
*            Program also creates a vector compatible dig_cat file
*            (besides the master),which contains names for each
*            GRASS category code; to save later key entry.
*
* modified:  removed as a stand alone program and merged into
*           the import.to.vect routines.
*  arguement list :
*  master-category-name, ascii-attr-file-name, binary-dlg-name, 
*   present directory, ARC/INFO dlg?(1[yes] or 0[no])
*
*                                     R.Glenn, SCS  7-26-88
*  ------Rev 4.+ arguements --------------------------------------
*    Input arguements:
*             dlgcat [-a]  dlg=binary-dlg-name
*                          att=ascii-attr-file-name
*                          subj=master-category-name
*
*    flags:
*         -a      : ARC/INFO dlg
*
*/
#include <stdio.h>
#include <ctype.h>
#include "gis.h"

main(argc, argv) 
	int argc ;
	char *argv[] ;
{
	int ier, cnt, i, k=0, l=0, cat_cnt=0, old_cat_cnt=0;
	int  record, reccnt=0, area_id, last_id=0;
	int  icode=0, dir, search, recd;
	char *nptr, *cptr, null='\0';
	char buffer [128], area_name[14], cat_name[14];
	char dum_name[66];
	char Input_name[60], Output_name[60], Category_name[60];
        char *mapset, *gets(), *tempname, *G_tempfile();
	char answer[30], Obuffer[30];
        struct Option *dlgopt, *attopt, *subjopt;
	struct Flag *a_flag;
	FILE *input_file, *output_file, *category_file, *tmp_file;

        G_gisinit (argv[0]);
     
		 /* set up the options and flags for the command line parser */
		  
	a_flag = G_define_flag();
	a_flag->key              = 'a';
	a_flag->description      = "Is this an ARC/INFO dlg";
					   
        dlgopt = G_define_option();
        dlgopt->key              = "dlg";
        dlgopt->type             =  TYPE_STRING;
        dlgopt->required         =  YES;
	dlgopt->description      = "dlg file to be imported";

        attopt = G_define_option();
        attopt->key              = "att";
        attopt->type             =  TYPE_STRING;
        attopt->required         =  YES;
	attopt->description      = "dlg.att file to be read";
					   
        subjopt = G_define_option();
        subjopt->key              = "subj";
        subjopt->type             =  TYPE_STRING;
        subjopt->required         =  YES;
	subjopt->description      = "SUBJ file to be used/created";

       /* heeeerrrrrre's the   PARSER */
        if (G_parser (argc, argv))
		exit (-1);
		    
		   /* start checking options and flags */

       /* open input, ascii attribute file  */
        sprintf (Input_name, "%s\0",attopt->answer);

       if((input_file = fopen (Input_name,"r")) == NULL)
           {
	   fprintf(stderr,"Can't open att file <%s> \n", Input_name) ;
	   exit (-1);
           }

/* open output, ascii file name with 2 appended 
*              will overwrite if it already exists  */
       sprintf (Output_name, "%s2\0",attopt->answer);
       if((output_file = fopen (Output_name,"w")) == NULL)
           {
	   fprintf(stderr,"Can't open att file <%s> \n", Output_name) ;
	   exit (-1);
           }

/* open category file for reading, if it exists*/
	sprintf(Category_name,"%s",subjopt->answer);

  	tempname = G_tempfile() ;
  	if ((ier = access(Category_name,0)) != -1)
	 	{            /* the category file exists, copy categ. to tmp */
                category_file = fopen (Category_name,"r");
                tmp_file = fopen (tempname,"a");
			     /*  get current category count */
		    fgets (buffer, 72, category_file);
		    sscanf(buffer,"%*s%d%*s",&old_cat_cnt);
                             /*  read past next four records */
	        for (recd=0;recd<=3;++recd)
	            {
		    fgets (buffer, 72, category_file);
		    }        /*  copy remaining records to tmp */
	        for (recd=0;;++recd)
		    {
		    if (!fgets (buffer, 72, category_file)) goto cat_end;
		    fputs (buffer,tmp_file);
		    }
cat_end:        fclose (category_file);
		fclose (tmp_file);
		tmp_file = fopen (tempname,"r");
                }
/*	if ((ier = access(Category_name,0)) == -1) */
	else     tmp_file = fopen (tempname,"a");

/* for ARC/INFO dlg formats, there is NO area 1, add an area 1 no data record */
/* for ARC/INFO dlg attribute files, first record is for area 2
   this is a universe polygon, ignore this first record */
	   dir = 0;
	   if (a_flag->answer)   dir = -1;

/*fprintf(stderr,"\n\t Initial subject categ. count = %4d\n",old_cat_cnt); */
           cat_cnt = old_cat_cnt;
/* main process */
	    for (record=0;;++record)
	        {                    /* select attribute records */
		if (!fgets (buffer, 82, input_file)) goto AT_END;
                reccnt++;
/*fprintf(stderr,"\tlength input = %d\n\t%s\n",strlen(buffer),buffer);*/
 		sscanf (buffer,"%d%s%",&area_id,area_name);

/* Set up for checking sequence of records */
		if (record == 0) 
		   {
/* Check for ARC/INFO, ignore first attribute record, univ. poly */
		   if (a_flag->answer)
		      {
		      last_id = area_id;
		      continue;
		      }
                   last_id = area_id - 1;
		   }
/*	        if ((! a_flag->answer) && (record <= 1))
		  {
		  if (record == 0) continue;
		  else last_id = area_id-1;
		  }
		else last_id = area_id-1;*/

/* Check area number sequence in attribute file, should be sequencial */
		if ((last_id+1) != area_id)
		  {
		  fprintf(stderr,"\nArea value %4d out of sequence at record# %d\n\n",area_id,record+1);
		  exit(-1);
		  }
		last_id = area_id;
		nptr = area_name;
/* Check attribute name, should not contain punct characters */
		while (ispunct(*nptr++))
		   {
		   fprintf(stderr,"\nAttribute file contains punctuation characters at record# %d\n\n",record);
		   exit(-1);
		   }
                nptr = area_name;

		k = strlen(area_name);
/*fprintf(stderr,"record %d  %d Bytes in !%s!\n",record,k,nptr); */

	/* find input string in category file, assign category value to the
		    area_name based on category file record number*/
		search = 1;
		icode = 0;
		rewind (tmp_file);
                while ( search )
		       {
	               for (recd=0;;++recd)
	                    {		 /* category file search */
		            if (!fgets (buffer, 82, tmp_file)) goto at_end;
/*fprintf(stderr,"\tcat recd %d length input = %d\n\t%s\n",recd,strlen(buffer),buffer); */
 		            sscanf (buffer,"%d:%s%s%",&l,cat_name,dum_name);
			    cptr = cat_name;
/*fprintf(stderr," comp leng= %d  area |%s|  cat |%s|  catrec %d\n",k,nptr,cptr,recd);
  sleep (1);*/
	/* compare for match */
	       	            if (strcmp(nptr,cptr) == 0)
			      {       /* match, assigned already */
			      icode = l;
		  	      break;
			      }
			    }         /* end of category search */
at_end: 
  			   if (icode == 0)
			        {      /* no match, add this name */
				fclose (tmp_file);
           			tmp_file = fopen (tempname,"a");
				icode = l+1;
			        sprintf (Obuffer,"%d:%s\n\0",icode,nptr);
	                        fputs (Obuffer, tmp_file);
				fclose (tmp_file);
  	        	        if ((old_cat_cnt - cat_cnt) == 0) 
				   fprintf(stderr,"\t   Number of new categ. added :     ");
				cat_cnt++;
  				fprintf(stderr,"\b\b\b\b%4d",recd);
/*fprintf(stderr,"\t new code - cat (%d) !%s!, cnt %d, break\n",icode,nptr,cat_cnt);
  sleep(1);*/
           		        tmp_file = fopen (tempname,"r");
				break;
			        }
		            search = 0;
		      }             /* end while loop */
                sprintf(Obuffer,"%5d%14.5s\t%d\n\0",area_id,area_name,icode);
/*fprintf(stderr," count: %d output: %s\n",cat_cnt,Obuffer); */
	        fputs (Obuffer, output_file);
                }
 /* all done, good-bye */
      AT_END :
	 fprintf(stderr,"\n");
         fclose(input_file);
         fclose(output_file);
/*fprintf(stderr,"\tAscii attribute processing finished,  %d categories added\n",cat_cnt - old_cat_cnt);*/
/* make a final mapset master category file 
		put in first three records  */
 	 category_file = fopen(Category_name,"w");
	 rewind (category_file);
	 sprintf (Obuffer,"#%5d categories\n\0",cat_cnt);
	 fputs(Obuffer,category_file);
	 sprintf(Obuffer,"Title\n\0");
	 fputs(Obuffer,category_file);
	 sprintf(Obuffer,"\n0.00 0.00 0.00 0.00\n0:no data\n\0");
	 fputs(Obuffer,category_file);
	 rewind (tmp_file);
/*   now append the tmp_file data  */
	 for (recd=0;;++recd)
	   {
	   if (!fgets (buffer, 30, tmp_file)) goto tmp_end;
	   fputs (buffer,category_file);
	   }
tmp_end: fclose (tmp_file);
	 fclose (category_file);
/* remove the tmp_file 
         sprintf(buffer,"rm %s\n\0",tempname);
         system (buffer); */
	 exit(0);
}
