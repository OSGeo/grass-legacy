/* updated for GRASS 5 9/99 */
/*  @(#)do_select.c     1.1  2/26/90    RLG */
/*  @(#)main.c          1.0  2/26/91    RLG , for 4.0*/

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include "gis.h"

#define	B_DIG	"dig"

static char  *current_mapset ;
static char  *gbase ;

int ask_name(int, struct Categories *, char *);
int conv_file(int, struct Categories *, char *, char *);

static char *intro[] =
{
"",
"",
0};

int 
main (int argc, char *argv[])
{
    char command[1024];
    char list_name[40], name[40], cat_list[128], new_cat[30];
    char *mapset, *tmp_file, *G_tempfile();
    int nfiles;
    char prompt[80];
    int i, opt_d, opt_n, opt_t;
    struct Categories cats;

    gbase = G_gisbase() ;
    current_mapset = G_mapset() ;
    G_gisinit (argv[0]);
    G_clear_screen ();

    command[0] = '\0';
    sprintf(prompt,"%s",getenv("GISBASE"));
    strcat (command, prompt);
    strcat (command, "/bin/v.extract"); 


    fprintf (stderr,"\n\n This program allows you to extract, then create a new vector file from \n an existing one; by providing category names or category codes.\n\n");

    gbase = G_gisbase() ;
    current_mapset = G_mapset() ;

    opt_t = 0;
    while(opt_t == 0)
      {
      fprintf(stderr,"\n Enter the type of map (area, line, or site) [area] : ");
      fgets(prompt,60,stdin);
      prompt[strlen(prompt)-1] = '\0'; /* Trim the linefeed */
      if (strlen(prompt) == 0)  
	   {
	   
	   opt_t = 1;
	   break;
	   }
      else switch(*prompt)
	   {
	   case 'a' :
	   case 'A' : opt_t = 1;
	              break;
           case 'l' :
	   case 'L' : opt_t = 2;
	              break;
           case 's' :
	   case 'S' : opt_t = 3;
                      break;
           default  : fprintf(stderr,"\n **** INVALID type, re-enter\n");
                      sleep(2);
		      opt_t = 0;
		      break;
           }
      }

    if (opt_t == 1)
       {
       opt_d = 0;
       i = G_yes("\n\n Do you want common boundaries dissolved ?",0) ;
       if (i)
          {
          strcat (command, " -d");
          opt_d = 1;
          }
       }

    opt_n = 0;
    i = G_yes("\n Do you want to use category names ?",0) ;
    if (i )
       {
       if (opt_d)strcat (command, "n");
       else strcat (command, " -n");
       opt_n = 1;
       }

    sprintf (prompt, "Enter vector(digit) map");
    mapset = G_ask_old( prompt, name, B_DIG, "vector") ;
    if ( ! mapset)
	exit(0);

    G_read_vector_cats (name, mapset, &cats);
    strcat (command, " input=");
    strcat (command, name);


    sprintf (prompt,"Enter name for resultant vector(digit) map");
    mapset = G_ask_any (prompt, name, B_DIG, "vector",1);
    if ( ! mapset)
	exit(0);

    strcat (command, " output=");
    strcat (command, name);

    if (opt_t == 1) strcat (command, " type=area");
    else if (opt_t == 2) strcat (command, " type=line");
    else strcat (command, " type=site");

    G_clear_screen ();

    if (opt_n)
       {        /* using names NOT category numbers */
       i = G_yes("\n Do you want to use a file of label names?",0) ;
       if (i )
          {     /* use file of names */
          strcat (command, " file=");
          sprintf (prompt, "Enter label file name");
          mapset = G_ask_old( prompt, list_name, "", "ascii") ;
          if ( ! mapset)
	      exit(0);

          if (G__file_name(name, "", list_name, G_mapset()) == NULL)
	        G_fatal_error ("Could not find list file %s\n", list_name);

  	  tmp_file = G_tempfile() ;
          if (conv_file(1,&cats,name,tmp_file) > 0)  exit(-1);
          strcat (command, tmp_file);
          }
       else
          {    /* no file, get a list of names */
          strcat (command, " list=");
          if (ask_name(opt_t,&cats, cat_list) > 0) exit(-1);
          strcat (command, cat_list);
          }
       }
    else
       {       /* name option not in effect, wants to use cat. numbers */
       i = G_yes("\n Do you want to use a file of categories?",0) ;
       if (i )
          {     /* use file of names */
          strcat (command, " file=");
          sprintf (prompt, "Enter category file name");
          mapset = G_ask_old( prompt, list_name, "", "ascii") ;
          if ( ! mapset)
	      exit(0);

          if (G__file_name(name, "", list_name, G_mapset()) == NULL)
	        G_fatal_error ("Could not find list file %s\n", list_name);

  	  tmp_file = G_tempfile() ;
          if (conv_file(0,&cats,name,tmp_file) > 0)  exit(-1);
          strcat (command, tmp_file);
          }
       else
          {    /* no file, get a list of categories */
          strcat (command, " list=");
          cat_list[0] = '\0';
          while(1)
            {
            fprintf(stderr,"   Enter Category Number (<CR> to END): ") ;
            fgets (new_cat,8,stdin) ;
            new_cat[strlen(new_cat)-1] = '\0';  /* Trim the linefeed */
            if (!strlen(new_cat) ) break;
	    fprintf( stderr, "\nString Length is %d\n", strlen(new_cat) );
            if (strlen(cat_list) > 0) strcat(cat_list,",");
            strcat(cat_list,new_cat);
	    }
          strcat (command, cat_list);
          }
       }

    strcat (command, " new=");
    new_cat[0] = '\0';
    if (opt_d)         /* if user wants dissolve feature, get category number */
       {
       fprintf(stderr,"\n You must assign a category number for this map\n\n");
       while(1)
         {
         fprintf(stderr,"   Enter Category Number : ") ;
         fgets (new_cat,8,stdin) ;
         new_cat[strlen(new_cat)-1] = '\0'; /* Trim the linefeed */
         if (!strlen(new_cat) ) 
	    {
	    fprintf(stderr," Invalid category number\n");
	    sleep(1);
	    }
         else
	    {
	    sscanf(new_cat,"%d", &i);
	    if (i <= 0)
	       {
	       fprintf(stderr," Invalid category number\n");
	       sleep(1);
	       }
            strcat(command,new_cat);
	    break;
            }
         }   /* end of while */
       }     /* end if opt_d == 1 */
    else 
       strcat (command,"0");

    fflush (stdout);
    G_clear_screen ();
    fprintf (stderr,"\n Extraction process begins:\n");
    fprintf(stderr," %s\n",command);   sleep(15);
    system (command); 
    exit(0);
}

int 
ask_name (int type, struct Categories *pcats, char *list)
{
    int i, icode, recd;
    char area_name[40], cat_name[100], buffr[128];
    char *nptr, *cptr, *pntr1;

    list[0] = '\0';

    while (1)
      {
      if (type == 1) fprintf(stderr,"   Enter the area name (<CR> to END): ");
      else if (type == 2) fprintf(stderr,"   Enter the line name (<CR> to END): ");
      else fprintf(stderr,"   Enter the site name (<CR> to END): ");
      fgets (buffr,127,stdin) ;
      buffr[strlen(buffr)-1] = '\0'; /* Trim the linefeed */
      if (!strlen(buffr)) break;

      sscanf (buffr, "%s", area_name);
      nptr = area_name;

      icode = 0;
	/* find input string in category struct, want to be sure of the
	     labels user inputs */

      recd = pcats->num;             /* set the number of categories */
      for (i=0;i<recd;i++)                /* category search */
	{     /* cycle cat names, look for a match */
        cat_name[0] = '\0';
        strcat(cat_name,pcats->labels[i]); /* get a category label */
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
	   if (strlen(list) > 0) strcat(list,",");
           icode = i;  /*set icode to category code */
	   strcat(list,nptr);
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

int 
conv_file (int type, struct Categories *pcats, char *infile, char *outfile)
{
    int i, icode, recd, begin=0, pass=0;
    int  area_value;
    char area_name[40], cat_name[100], buffr[128], number[2];
    char *nptr, *cptr, *pntr1;
    FILE *IN, *OUT;

    /* open input file */
    IN = fopen(infile,"r");

    /* open output will overwrite if it already exists  */
    OUT = fopen (outfile,"w");
    rewind (OUT);

    if (type) fprintf(stderr,"	Checking label names in file <%s> ....\n", infile);
    else fprintf(stderr,"	Checking category values in file <%s> ....\n", infile);

    recd = pcats->num;             /* set the number of categories */

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
           strcat(cat_name,pcats->labels[i]); /* get a category label */
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
              icode = i; /* set icode to category code */
  	      sprintf(area_name,"%s\n",nptr);
              fputs(area_name,OUT);
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

         sscanf (buffr, "%d", &area_value);
         icode = 0;
	   /* find input value in category struct, want to check validity */

         for (i=0;i<recd;i++)                /* category search */
	   {     /* cycle cat names, look for a match */
/*fprintf(stderr,"area= %d, cat= %d\n",area_value, i); */
              if (area_value == i) 
		 {
                 icode = i; 
  	         sprintf(number,"%d\n",area_value);
                 fputs(number,OUT);
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

   fclose(IN);
   fclose(OUT);
   return(0);
}
