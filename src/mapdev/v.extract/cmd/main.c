/*
 * $Id$
 * updated by David D Gray <ddgray@armadce.demon.co.uk> 4/2000 
 * updated GRASS 5 Bill Hughes 9/99
 * main.c    1.0   10/01/89
 * main.c    1.1   1/30/91
*    Created by : R.L.Glenn , Soil Conservation Service, USDA
*    Purpose: Productivity tool
*	      Provides a means of generating vector (digit) files
*             from an existing vector maplayer. Selects all vector
*             boundaries for 1 or several areas of a list of
*             user provided categories.
*
*
*  ------Rev 4.+ arguements --------------------------------------
*    Input arguements:
*             v.extract [-dn]  input=vector file to read 
*                              output=vector file to create
*                              list=list of categories 
*                                        separated by commas
*                              new=new category value or 0
*                              type=area,line, or site
*                              [file=category label file]
*
*    flags:
*         -d      : dissolve common boundaries
*         -n      : use category names, NOT numbers
*
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include  "gis.h"
#include "Vect.h"
#include "local_proto.h"

#define		B_DIR		"dig"
#define		A_DIR		"dig"
#define		ATT_DIR		"dig_att"
#define		CAT_DIR		"dig_cats"

struct Map_info Map;
int cat_array[5000];
char  buf[1024] ;
struct dig_head Head;


int main (int argc, char **argv)
{
	int i, ier, cat_index, new_cat, max_att;
	int cat_count, morecats;
	int result;
	int dissolve=0, cnt, x, y;
        char buffr[80], file_name[80], text[80];
        char *input, *output, *mapset;
        struct Categories cats, temp_cats;
        struct Option *inopt, *outopt, *fileopt, *newopt, *typopt, *listopt;
        struct Flag *d_flag, *n_flag;
        FILE *in, *catf;


            /* set up the options and flags for the command line parser */

    d_flag = G_define_flag();
    d_flag->key              = 'd';
    d_flag->description      = "Dissolve common boundaries (default is no) ";
    
    n_flag = G_define_flag();
    n_flag->key              = 'n';
    n_flag->description      = "Use category names NOT numbers ";

    typopt = G_define_option();
    typopt->key              = "type";
    typopt->type             =  TYPE_STRING;
    typopt->required         =  YES;
    typopt->options          =  "area,line,site";
    typopt->description      =  "Select area, line, or site "; 

    inopt = G_define_option();
    inopt->key             = "input";
    inopt->type            = TYPE_STRING;
    inopt->required        = YES;
    inopt->gisprompt       = "old,dig,vect";
    inopt->description     = "vector input map name ";

    outopt = G_define_option();
    outopt->key             = "output";
    outopt->type            =  TYPE_STRING;
    outopt->required        =  YES;
    outopt->gisprompt       = "any,dig,vect";
    outopt->description     = "vector output map name ";

    newopt = G_define_option();
    newopt->key              = "new";
    newopt->type             =  TYPE_INTEGER;
    newopt->required         =  YES;
    newopt->description      = "Enter 0 or a desired NEW category value ";

    listopt = G_define_option();
    listopt->key             = "list";
    listopt->type            =  TYPE_STRING;
    listopt->required        =  NO;
    listopt->multiple        =  YES;
    listopt->key_desc        = "range";
    listopt->description     = "Category ranges: e.g. 1,3-8,13\n           Category list: e.g. Abc,Def2,XyZ " ;

    fileopt = G_define_option();
    fileopt->key             = "file";
    fileopt->type            =  TYPE_STRING;
    fileopt->required        =  NO;
    fileopt->description     = "Text file name for category range/list ";


    G_gisinit (argv[0]);


       /* heeeerrrrrre's the   PARSER */
    if (G_parser (argc, argv))
        exit (-1);

       /* start checking options and flags */

    if (listopt->answers == NULL && fileopt->answer == NULL)
	{
        	fprintf(stderr,"\nEither [list] or [file] should be given.\n");
		exit(1);
	}

       /* set input vector file name and mapset */
    input = inopt->answer;
    mapset = G_find_vector (input, "") ;
    cat_index = 0;
    if (mapset == NULL)
	{
		sprintf(buffr,"Vector file [%s] not available in search list",
		    input);
		G_fatal_error(buffr) ;
	}
      
       /* set output vector file name */
    output = outopt->answer;

    if ( d_flag->answer ) dissolve = 1;
    if ( !*newopt->answer) new_cat = 0;
    else new_cat = atoi(newopt->answer);

    if ( n_flag->answer )    /* check name flag */
      {    
              /* the n_flag is set, read in the categories for this file */
      G_read_vector_cats (input, mapset, &cats);
              /* check for a file of category names */
      if ( fileopt->answer == NULL )
         {
            /* no file of names to read, process name list */
                  /* first check for validity */
         for (i = 0; listopt->answers[i]; i++)
            if (!scan_names (&cats, listopt->answers[i], &x))
	    {
            fprintf(stderr,"\nCategory label <%s> not found\n",
                              listopt->answers[i]);
		exit(1);
	    }
                  /* valid names, put this into a values array */
         for (i = 0; listopt->answers[i]; i++)
            {
            scan_names (&cats, listopt->answers[i], &x);
            cat_array[cat_index] = x; cat_index++; 
            }
         }
      else /* got file of category names */
         {
	 sprintf(file_name,"%s",fileopt->answer);

            /* open input file */
         if( (in = fopen(file_name,"r")) == NULL )
	   {
	   fprintf(stderr,"Can't open name file <%s> \n", file_name) ;
	   exit (-1);
	   }

         while (1)
           {
           if (!fgets (buffr, 39, in)) break;
           sscanf (buffr, "%s", text);
           scan_names (&cats, text, &x);
           cat_array[cat_index] = x; cat_index++; 
           }
         fclose(in);
         }
      }
    else   /* NO name flag */
      {    /* check for a file of categories */
      if ( fileopt->answer == NULL )
         {
             /* no file of categories to read, process cat list */

            /* first check for valid list */
         for (i = 0; listopt->answers[i]; i++)
            if (!scan_cats (listopt->answers[i], &x, &y))
	    {
            fprintf(stderr,"\nCategory value in <%s> not valid\n",
                              listopt->answers[i]);
		exit(1);
	    }

            /* valid list, put into cat value array */
	 cat_index = 0;
         for (i = 0; listopt->answers[i]; i++)
            {
            scan_cats (listopt->answers[i], &x, &y);
            while (x <= y)
               {
               cat_array[cat_index] = x++; cat_index++; 
               } 
            }
         }
      else  /* got a file of category numbers */
         {
	 sprintf(file_name,"%s",fileopt->answer);
         fprintf(stderr,"process file <%s> for cats\n",file_name);

            /* open input file */
         if( (in = fopen(file_name,"r")) == NULL )
	   {
	   fprintf(stderr,"Can't open category file <%s> \n", file_name) ;
	   exit (-1);
	   }
         while (1)
           {
           if (!fgets (buffr, 39, in)) break;
           sscanf(buffr, "%s", text);
           scan_cats (text, &x, &y);
	   cat_index = 0;
           while (x <= y)
              {
              cat_array[cat_index] = x++; cat_index++; 
              } 
           }
         fclose(in);
         }
      }
/* for (i=0; i<cat_index;i++)
 fprintf(stderr,"cat-array[%d]= %d\n",i,cat_array[i]);*/

    if (*typopt->answer == 'a')
       {
       max_att = xtract_area(cat_index,cat_array,input,output,dissolve,new_cat);
       if ( 0 > max_att)
          {
          fprintf(stderr," Error in area extraction processing\n");
          exit(1);
          }
       }
    else
       {
       max_att = xtract_line(cat_index,cat_array,input,output,new_cat);
       if ( 0 > max_att)
          {
          fprintf(stderr," Error in line/site extraction processing\n");
          exit(1);
          }
       }
                     /* Open output "dig_cats" file */
    /* G__file_name(file_name, CAT_DIR, output, G_mapset()) ; */
    result = G_init_cats( (CELL)0, "", &temp_cats);
    fprintf( stderr, "Result of cats initialisation is %d\n", result );
    /* if ( (catf = fopen (file_name, "w")) == NULL)
       {
       fprintf(stderr,"Can't create output dig_cats file <%s> \n", file_name) ;
       return (-1);
       } */
    G_set_cats_title( output, &temp_cats );
    G_set_cat( 0, "no data", &temp_cats);

    fprintf (stdout,"\n");
    fprintf (stdout,"    Making category file\n");

                      /* make a cats file */
    /* if (dissolve)
       sprintf(buffr,"# %d categories\nTitle %s\n",new_cat,output);
    else
       sprintf(buffr,"# %d categories\nTitle %s\n",max_att,output);
    fputs(buffr,catf);
    sprintf(buffr,"\n0.00 0.00 0.00 0.00\n0:no data\n");
    fputs(buffr,catf);
    */

    if (dissolve) {
      /* for (i = 1; i <= new_cat; i++)
           {
           sprintf(buffr,"%d:\n",i);
           fputs(buffr,catf);
           }
      */
      if( new_cat == 0 ) new_cat = cat_array[0];
      if (G_read_vector_cats(input, mapset, &cats) == 0)
	G_set_cat( new_cat, G_get_cat(new_cat, &cats), &temp_cats);
      else
	G_set_cat(new_cat, "", &temp_cats);
    }
     else
	 {
		      /* first try reading parent cat file data */
	   if (G_read_vector_cats(input, mapset, &cats) == 0) 
	     {
	       if(new_cat == 0)
		 {
		   cat_count = 0;
		   while(cat_array[cat_count])
		     {
		       G_set_cat( cat_array[cat_count], G_get_cat(cat_array[cat_count],
								  &cats), &temp_cats );
		       cat_count++;
		     }
		 }
	       else
		 G_set_cat(new_cat, G_get_cat(new_cat, &cats), &temp_cats);
	     }
         else   /* build an empty cat file */
	    {
	       if(new_cat == 0)
		 {
		   cat_count = 0;
		   while(cat_array[cat_count])
		     {
		       G_set_cat( cat_array[cat_count], "", &temp_cats );
		       cat_count++;
		     }
		 }
	       else
		 G_set_cat(new_cat, "", &temp_cats);
	    }
	 }
     if( G_write_vector_cats( output, &temp_cats ) < 0 )
       fprintf( stderr, "Could not write category file, see error above." );

     sprintf( buffr, "%s/etc/v.build  map=%s  thresh=no",G_gisbase(),output);
     system(buffr);

		       /* give the user this message  */
     fprintf(stderr, "\n\nExtracted vector file <%s> has been created in the 'dig' directory\n\n",output);

     exit(0);
}

int 
scan_cats (char *s, int *x, int *y)
{
    char dummy[2];

    *dummy = 0;
    if (sscanf (s, "%d-%d%1s", x, y, dummy) == 2)

	return (*dummy == 0 && *x <= *y);
    *dummy = 0;
    if (sscanf (s, "%d%1s", x, dummy) == 1 && *dummy == 0)
   {
	*y = *x;
	return 1;
    }
    return 0;
}


int scan_names (struct Categories *pcats, char *s, int *x)
{
    int i, icode, recd;
    char area_name[40], cat_name[40], buff[128];
    char *nptr, *cptr, *pntr1;
    char dummy[2];

    *dummy = 0;
    sscanf (s, "%s%1s", area_name, dummy);
      nptr = area_name;

      icode = 0;
	/* find input string in category struct, assign category value to the
		    area_name based on category file record number*/

      recd = pcats->num;             /* set the number of categories */
      for (i=0;i<recd;i++)                /* category search */
	{     /* cycle cat names, look for a match */
        cat_name[0] = '\0';
        strcat(cat_name,pcats->labels[i]); /* get a category label */
        pntr1 = cat_name;
        cptr = buff;
        *cptr = '\0';  
        while (1)
           {   /* look for cats field separator SCS version */
           if (ispunct(*pntr1) || *pntr1 == '\0') break;
           *(cptr) = *(pntr1);
           pntr1++; cptr++;
           }
        *cptr = '\0';  cptr = buff;
/*fprintf(stderr,"i= %d, compare nam|%s| :cat|%s|, num= %d\n",i,nptr,cptr,pcats->list[i].num);
  fprintf(stderr,"       compare value= %d\n",strcmp(cptr,nptr));
  sleep(2);*/
	if (strcmp(nptr,cptr) == 0)     /* compare for match */
	   {                           /* match, assigned already */
           *x = i ; /* return category code */
           return(1);
	   }
	} 
	/* end of category search for loop */

      return(0);
}
