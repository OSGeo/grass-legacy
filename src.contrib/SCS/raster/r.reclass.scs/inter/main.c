/* %W%   %G% */
/* @(#)main.c	        1.0   02/27/91 
/*    created - RL.Glenn, SCS 
*     to allow names or categories for reclassification
*     to allow file input
*/
#include <ctype.h>
#include "gis.h"

#define	RAST	"cell"

static char  *current_mapset ;
static char  *gbase ;

static char *intro[] =
{
"",
"",
0};

main(argc, argv)
    int argc ;
    char *argv[] ;
{
    char command[1024], buf[128]  ;
    char prompt[80], prompt2[80];
    char list_name[40], name[40], cat_list[128], new_cat[10];
    int i, type, n_flag, f_flag ;
    char *mapset, *gets(), *tmp_file, *G_tempfile();
    struct Categories cats;
    FILE *TEMP;

    gbase = G_gisbase() ;
    current_mapset = G_mapset() ;
    G_gisinit (argv[0]);
    G_clear_screen ();

    command[0] = '\0';
    sprintf(prompt,"%s\0",getenv("GISBASE"));
    strcat (command, prompt);
    strcat (command, "/bin/r.reclass"); 

    sprintf (prompt, "Enter raster map");
    mapset = G_ask_old( prompt, name, RAST, "raster") ;
    if ( ! mapset)
	exit(0);

    G_read_cats (name, mapset, &cats);
    strcat (command, " input=");
    strcat (command, name);

    sprintf (prompt,"Enter name for reclass raster map");
    mapset = G_ask_any (prompt, name, RAST, "raster",1);
    if ( ! mapset)
	exit(0);

    strcat (command, " output=");
    strcat (command, name);

    strcat (command, " < ");
    tmp_file = G_tempfile() ;
    strcat (command, tmp_file);

    while(1)               /* reclass data collection */
       {
       G_clear_screen();
       fprintf(stderr,"\n\nRECLASSIFICATION CATEGORY INPUT\n\n");

       n_flag = G_yes("\n Do you want to use category names ?",0) ;
       if (n_flag) 
          sprintf(prompt,"\n Do you want to use a file of names ? ");
       else
          sprintf(prompt,"\n Do you want to use a file of categories ? ");

       f_flag = G_yes(prompt,0) ;
       if (f_flag && n_flag)
	  sprintf(prompt2,"\n Do you have another file of names ? ");
       if (f_flag && !n_flag)
	  sprintf(prompt2,"\n Do you have another file of categories ? ");
       if (! f_flag && n_flag)
	  sprintf(prompt2,"\n Do you have another list of names ? ");
       if (!f_flag && !n_flag)
	  sprintf(prompt2,"\n Do you have another list of categories ? ");

       if (f_flag )
          {                        /* use file */
          if (n_flag) sprintf (prompt, " Enter label file name");
          else (prompt, " Enter category file name");
          mapset = G_ask_old( prompt, list_name, "", "ascii", 0) ;
          if ( ! mapset)
	         exit(0);

          if (G__file_name(name, "", list_name, G_mapset()) == NULL)
	           G_fatal_error ("Could not find list file %s\n", list_name);

          if (conv_file(n_flag,&cats,name,tmp_file) > 0)  exit(-1);
          }
       else        /* !f_flag, use list  */
          {                      
	  if (n_flag)            /* !f_flag && n_flag   list of names */
	    {
            if (ask_name(&cats, cat_list) > 0) exit(-1);
            fprintf(stderr,"\n Enter a reclass category value : ");
            gets(prompt);
            if (strlen(prompt) == 0)
               {
               fprintf(stderr,"   NO value entered, quitting \n");
               exit(-1);
               }
            sscanf(prompt,"%d",&i);
            sprintf(buf," = %d\n",i);
	    strcat(cat_list,buf);
            TEMP = fopen (tmp_file,"a");
fprintf(stderr,"end list= <%s>\n",cat_list);
            fputs(cat_list,TEMP);
	    fclose(TEMP);
	    }
          else
            {                     /* !f_flag && !n_flag  list of cat. numbers */
            TEMP = fopen (tmp_file,"a");
            cat_list[0] = '\0';
            while(1)
              {           /* get categories from user */
              fprintf(stderr,"   Enter Category Number (<CR> to END): ") ;
              gets (prompt) ;
              if (!strlen(prompt) ) 
	         {
                 fprintf(stderr,"\n Enter a reclass category value : ");
                 gets(prompt);
                 if (strlen(prompt) == 0)
                    {
                    fprintf(stderr,"   NO value entered, quitting \n");
		    fclose(TEMP);
                    exit(-1);
                    }
                 sscanf(prompt,"%d",&i);
                 sprintf(buf," = %d\n",i);
	         strcat(cat_list,buf);
fprintf(stderr,"end list= <%s>\n",cat_list);
                 fputs(cat_list,TEMP);
		 fclose(TEMP);
	         break;
	         }
              sprintf(buf," %s",prompt);
              strcat(cat_list,buf);
	      }
	    }       /* end of cat. number collection */
          }         /* end of list collection */
       
       i = G_yes(prompt2,0) ;
       if (! i ) 
	  {
	  TEMP = fopen (tmp_file,"a");
	  sprintf(buf,"end\n");
          fputs(buf,TEMP);
	  fclose(TEMP);
	  break;
	  }
       }        /* end of while collecting reclass data */
    
    fflush (stdout);
    G_clear_screen ();
    fprintf (stderr,"\n reclass process begins:\n");
/*  fprintf(stderr," %s\n",command);   sleep(4);*/
    system (command); 
    exit(0);
}

conv_file(type, pcats,infile, outfile)
    int type;
    char *infile, *outfile;
    struct Categories *pcats ;
{
    int i, icode, recd, begin=0, pass=0;
    int  area_value, cat_value;
    char area_name[40], cat_name[40], class_codes[128], buffr[128], number[10];
    char *nptr, *cptr, *pntr1, *gets() ;
    FILE *IN, *OUT;

    /* open input file */
    IN = fopen(infile,"r");

    /* open output will overwrite if it already exists  */
    OUT = fopen (outfile,"a");

    fprintf(stderr,"\n Enter a reclass category value : ");
    gets(buffr);
    if (strlen(buffr) == 0)
       {
       fprintf(stderr,"   NO value entered, quitting \n");
       fclose(IN);
       fclose(OUT);
       return(1);
       }
    sscanf(buffr,"%d",&cat_value);
    class_codes[0] = '\0';

    if (type) fprintf(stderr,"	Checking label names in file\n\t <%s> ....\n", infile);
    else fprintf(stderr," Checking category values in file\n\t <%s> ....\n", infile);

    recd = pcats->count;             /* set the number of categories */

    if (type)
       {
       while (1)
         {
         if (!fgets (buffr, 39, IN)) break;

         sscanf (buffr, "%s", area_name);
         nptr = area_name;

         icode = 0;
	   /* find input string in category struct, want to check validity */

         for (i=0;i<recd;i++)                /* category search */
	   {     /* cycle cat names, look for a match */
           cat_name[0] = '\0';
           strcat(cat_name,pcats->list[i].label); /* get a category label */
           pntr1 = cat_name;
           cptr = buffr;
           *cptr = '\0';  
           while (1)
              {   /* look for cats field separator SCS version */
              if (ispunct(*pntr1) || *pntr1 == '\0') break;
              *(cptr) = *(pntr1);
              pntr1++; cptr++;
              }
           *cptr = '\0';  cptr = buffr;
/*fprintf(stderr,"i= %d, compare nam|%s| :cat|%s|\n",i,nptr,cptr);
  fprintf(stderr,"       compare value= %d\n",strcmp(cptr,nptr));*/
	   if (strcmp(nptr,cptr) == 0)     /* compare for match */
	      {                           /* match, assigned already */
              icode = pcats->list[i].num; /* set icode to category code */
  	      sprintf(number," %d",icode);
              strcat(class_codes,number);
	      break;
	      }
	   } /* end of category search for loop */
	/* end of category search, NO category names match */

         if (!icode) 
            {
            fprintf(stderr,"\nCategory label <%s> not found\n",area_name);
	    sleep(2);
            fclose(IN);
            fclose(OUT);
            return(1);
            }

         }   /* end of while */
       }    /* end of type 1 (names) check */
    else
       {
       while (1)
         {
         if (!fgets (buffr, 39, IN)) break;

         sscanf (buffr, "%d", area_value);
         icode = 0;
	   /* find input value in category struct, want to check validity */

         for (i=0;i<recd;i++)                /* category search */
	   {     /* cycle cat names, look for a match */
/*fprintf(stderr,"area= %d, cat= %d\n",area_value, pcats->list[i].num); */
              if (area_value == pcats->list[i].num) 
		 {
                 icode = pcats->list[i].num; 
  	         sprintf(number,"%d\n",area_value);
		 strcat(class_codes,number);
	         break;
		 }
	   } /* end of category search for loop */
	/* end of category search, NO category value match */

         if (!icode) 
            {
            fprintf(stderr,"\nCategory value <%d> not found\n",area_value);
	    sleep(2);
            fclose(IN);
            fclose(OUT);
            return(1);
            }

         }   /* end of while */
       }    /* end of type 2 (cat values) check */

    sprintf(buffr," = %d\n",cat_value);
    strcat(class_codes,buffr);
    fputs(class_codes,OUT);
    fclose(IN);
    fclose(OUT);
    return(0);
}

ask_name(pcats,list)
    char *list;
    struct Categories *pcats ;
{
    int i, icode, recd;
    char area_name[40], cat_name[40], buffr[128];
    char *nptr, *cptr, *pntr1, *gets() ;

    list[0] = '\0';

    while (1)
      {
      fprintf(stderr,"   Enter the category name (<CR> to END): ");
      gets (buffr) ;
      if (!strlen(buffr)) break;

      sscanf (buffr, "%s", area_name);
      nptr = area_name;

      icode = 0;
	/* find input string in category struct, want to be sure of the
	     labels user inputs */

      recd = pcats->count;             /* set the number of categories */
      for (i=0;i<recd;i++)                /* category search */
	{     /* cycle cat names, look for a match */
        cat_name[0] = '\0';
        strcat(cat_name,pcats->list[i].label); /* get a category label */
        pntr1 = cat_name;
        cptr = buffr;
        *cptr = '\0';  
        while (1)
           {   /* look for cats field separator SCS version */
           if (ispunct(*pntr1) || *pntr1 == '\0') break;
           *(cptr) = *(pntr1);
           pntr1++; cptr++;
           }
        *cptr = '\0';  cptr = buffr;
/*fprintf(stderr,"i= %d, compare nam|%s| :cat|%s|\n",i,nptr,cptr);
  fprintf(stderr,"       compare value= %d\n",strcmp(cptr,nptr));*/
	if (strcmp(nptr,cptr) == 0)     /* compare for match */
	   {                           /* match, assigned already */
           icode = pcats->list[i].num;  /*set icode to category code */
	   sprintf(buffr," %d",icode);
	   strcat(list,buffr);
	   }
	}        /* end of category search for loop */

      if (!icode)
	 {
         fprintf(stderr,"Category label <%s> not found\n",nptr);
	 sleep(2);
	 }
      }   /* end of while */
return(0);
}
