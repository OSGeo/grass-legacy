/* %W% %G% */
/*  @(#)do_select.c     1.1  2/26/90    RLG */
/*  @(#)main.c          1.0  2/26/91    RLG , for 4.0*/

#include <ctype.h>
#include "gis.h"

#define	B_DIG	"dig"

static char  *current_mapset ;
static char  *gbase ;

static char *intro[] =
{
"",
"",
0};

main (argc, argv) char *argv[];
{
    char command[1024], path[256];
    char list_name[40], name[40], cat_list[128], new_cat[10];
    char in_name[20], out_name[20], tmp_dig[20];
    char *mapset, *gets(), *tmp_file, *G_tempfile();
    int x, y;
    char prompt[80];
    int i, uniq, opt_d, opt_n, opt_t, opt_f;
    struct Categories cats;
    FILE *TMP;

    gbase = G_gisbase() ;
    current_mapset = G_mapset() ;
    G_gisinit (argv[0]);
    G_clear_screen ();

    command[0] = '\0';
    sprintf(prompt,"%s\0",getenv("GISBASE"));
    strcat (command, prompt);
    strcat (command, "/bin/v.reclass"); 


    fprintf (stderr,"\n\n This program allows you to reclassify an existing vector file \n  and create a new file; by providing category names or category codes.\n\n");

    gbase = G_gisbase() ;
    current_mapset = G_mapset() ;

    opt_t = 0;
    while(opt_t == 0)
      {
      fprintf(stderr,"\n Enter the type of map (area, line, or site) [area] : ");
      gets(prompt);
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
    if (i ) opt_n = 1;

    sprintf (prompt, "Enter vector(digit) map");
    mapset = G_ask_old( prompt, name, B_DIG, "vector") ;
    if ( ! mapset)
	exit(0);
    sprintf(in_name,"%s",name);
    strcat (command, " input=");
    strcat (command, in_name);

    G_read_vector_cats (name, mapset, &cats);

    sprintf (prompt,"Enter name for resultant vector(digit) map");
    mapset = G_ask_any (prompt, name, B_DIG, "vector",1);
    if ( ! mapset)
	exit(0);
    sprintf(out_name,"%s",name);
    strcat (command, " output=");
    strcat (command, out_name);

    if (opt_t == 1) strcat (command, " type=area");
    else if (opt_t == 2) strcat (command, " type=line");
    else strcat (command, " type=site");

    opt_f = 0;
    if (opt_n)         /* using names NOT category numbers */
       i = G_yes("\n Do you want to use a file of label names?",0) ;
    else
       i = G_yes("\n Do you want to use a file of categories?",0) ;
    if (i ) opt_f = 1;

    uniq = 0;
    tmp_file = G_tempfile() ;
    strcat (command, " file=");

    for (;;)      /* build the conversion file */
    {
    G_clear_screen ();
    uniq++;
    fprintf(stderr,"\n\tClass %d\n",uniq);
    if (opt_n)
       {        /* using names NOT category numbers */
       if (opt_f)
          {     /* using file of names */
          sprintf (prompt, "Enter label file name");
          mapset = G_ask_old( prompt, list_name, "", "ascii", 0) ;
          if ( ! mapset)
	      exit(0);

          sprintf(path,"%s/%s/%s",G_location_path(),mapset,list_name);
	  if (access(path,0) != 0)
	        G_fatal_error ("Could not find list file %s\n", list_name);

          if (conv_file(1,&cats,path,list_name,tmp_file,uniq) > 0)  exit(-1);
          }
       else
          {    /* no file, get a list of names */
          if (ask_name(opt_t,&cats, tmp_file, uniq) > 0) exit(-1);
          }
       }
    else
       {       /* name option not in effect, wants to use cat. numbers */
       if (opt_f )
          {     /* use file of names */
          sprintf (prompt, "Enter category file name");
          mapset = G_ask_old( prompt, list_name, "", "ascii", 0) ;
          if ( ! mapset)
	      exit(0);

	  if (access(list_name,0) != 0)
	        G_fatal_error ("Could not find list file %s\n", list_name);
          sprintf(path,"%s/%s/%s",G_location_path(),mapset,list_name);
	  if (access(path,0) != 0)
	        G_fatal_error ("Could not find list file %s\n", list_name);


          if (conv_file(0,&cats,path,list_name,tmp_file,uniq) > 0)  exit(-1);
          }
       else
          {    /* no file, get a list of categories */
          cat_list[0] = '\0';
               /* open output will append if it already exists  */
          TMP = fopen (tmp_file,"a");

          while(1)
            {
            fprintf(stderr,"   Enter Category Number (<CR> to END): ") ;
            gets (new_cat) ;
            if (!strlen(new_cat) ) break;
            scan_cats (new_cat, &x, &y);
            while (x <= y)
              {
  	      sprintf(cat_list,"%d:%d\n",x++,uniq);
              fputs(cat_list,TMP);
              } 
	    }
          fclose (TMP);
          }
       }
    fprintf (stderr,"\n This re-classification pass is complete ");
    i = G_yes("\n Do you want to do another ?",0) ;
    if (!i) 
      break;
    }
    strcat (command, tmp_file);
    fflush (stdout);

    G_clear_screen ();
    fprintf (stderr,"\n Re-classification process begins:\n");
/*  fprintf(stderr," %s\n",command); */
    system (command); 

    unlink(tmp_file);
    exit(0);
}

ask_name(type, pcats,outfile, clas_num)
    int type, clas_num;
    char *outfile;
    struct Categories *pcats ;
{
    int i, icode, recd;
    char area_name[40], cat_name[100], buffr[128], obuff[10];
    char *nptr, *cptr, *pntr1, *gets() ;
    FILE *OUT;

    /* open output will append if it already exists  */
    OUT = fopen (outfile,"a");

    while (1)
      {
      if (type == 1) fprintf(stderr,"   Enter the area name (<CR> to END): ");
      else if (type == 2) fprintf(stderr,"   Enter the line name (<CR> to END): ");
      else fprintf(stderr,"   Enter the site name (<CR> to END): ");
      gets (buffr) ;
      if (!strlen(buffr)) break;

      sscanf (buffr, "%[^\n]", area_name);
      nptr = area_name;

      icode = 0;
	/* find input string in category struct, want to be sure of the
	     labels user inputs */

      recd = pcats->count;             /* set the number of categories */
      for (i=0;i<recd;i++)                /* category search */
	{     /* cycle cat names, look for a match */
        cat_name[0] = '\0';
        strcat(cat_name,pcats->list[i].label); /* get a category label */
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
/*fprintf(stderr,"i= %d, compare nam|%s| :cat|%s|\n",i,nptr,cptr);
  fprintf(stderr,"       compare value= %d\n",strcmp(cptr,nptr));*/
	if (strcmp(nptr,cptr) == 0)     /* compare for match */
	   {                           /* match, assigned already */
           icode = pcats->list[i].num;  /*set icode to category code */
  	   sprintf(obuff,"%d:%d\n",icode,clas_num);
           fputs(obuff,OUT);
	   }
	}        /* end of category search for loop */

      if (!icode)
	 {
         fprintf(stderr,"Category label <%s> not found\n",nptr);
	 sleep(2);
	 }
      }   /* end of while */
   
    fclose(OUT);
    return(0);
}

conv_file(type, pcats,inpath,infile, outfile, clas_num)
    int type, clas_num;
    char *inpath, *infile, *outfile;
    struct Categories *pcats ;
{
    int i, icode, recd, begin=0, pass=0;
    int  area_value;
    char area_name[40], cat_name[100], buffr[128], obuff[10];
    char *nptr, *cptr, *pntr1, *gets() ;
    FILE *IN, *OUT;

    /* open input file  will append if it exists*/
    IN = fopen(inpath,"r");

    /* open output will append if it already exists  */
    OUT = fopen (outfile,"a");

    if (type) fprintf(stderr,"	Checking label names in file <%s> ....\n", infile);
    else fprintf(stderr,"	Checking category values in file <%s> ....\n", infile);

    recd = pcats->count;             /* set the number of categories */

    if (type)
       {
       while (1)
         {
         if (!fgets (buffr, 39, IN)) break;

         sscanf (buffr,"%[^\n]",area_name); 
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

         icode = 0;
	   /* find input string in category struct, want to check validity */

         for (i=0;i<recd;i++)                /* category search */
	   {     /* cycle cat names, look for a match */
           cat_name[0] = '\0';
           strcat(cat_name,pcats->list[i].label); /* get a category label */
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
 
/*fprintf(stderr,"i= %d, compare nam|%s| :cat|%s|\n",i,nptr,cptr);
  fprintf(stderr,"       compare value= %d\n",strcmp(cptr,nptr));
  sleep(1); */
	   if (strcmp(nptr,cptr) == 0)     /* compare for match */
	      {                           /* match, assigned already */
              icode = pcats->list[i].num; /* set icode to category code */
  	      sprintf(obuff,"%d:%d\n",icode,clas_num);
              fputs(obuff,OUT);
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
/*fprintf(stderr,"area= %d, cat= %d\n",area_value, pcats->list[i].num); */
              if (area_value == pcats->list[i].num) 
		 {
                 icode = pcats->list[i].num; 
  	         sprintf(obuff,"%d:%d\n",icode,clas_num);
                 fputs(obuff,OUT);
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

scan_cats (s, x, y)
    char *s;
    int *x, *y;
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

