/*  @(#)ask_name.c    1.1  04/90  */
/*
**  Written by: R.L.Glenn, SCS, 4/1990
*/

#include "digit.h"
#include "gis.h"
#include <stdio.h>

ask_for_name(Type,pcats)
    int Type;
    struct Categories *pcats ;
{
    int ans, ans2, ier;
    char buffr[128];
    char *title;

    ans = -1;
    while (ans < 0)
       {
       Clear_info ();
       if (Type == 1)
	  ans = curses_yes_no_default(2,
			" Do you wish to enter line names? ", 1);
       else if (Type == 2)
	  ans = curses_yes_no_default(2,
			" Do you wish to enter area names? ", 1);
       else 
	  ans = curses_yes_no_default(2,
			" Do you wish to enter site names? ", 1);
       if (ans)
	 {
                                 /* Make Master Category dir, if not existing */
         G__make_mapset_element("SUBJ") ;
	 while(1)
	   {
	   ans = -1;
	   while(ans == -1)
	      {
              Clear_info ();
              Write_info (2,
		" Enter the SUBJECT matter ");
	      Write_info (3,
	        " Enter 'list' for available Subject files");
	      Write_info (4,
	        " <CR> to Abort/Quit): ");
              Get_curses_text(buffr,20);
              if (strlen(buffr) == 0) ans = 0;
	      else if (strcmp(buffr,"list") == 0) list("SUBJ");
	      else ans = 1;
	      }
                       
	   if (ans == 0) break;
	   N_subj_file = G_store(buffr);

                                          /* read category file , if it exists*/
           G_suppress_warnings (1);
           ier = read_cats ("SUBJ", N_subj_file, G_mapset(), pcats, 1);
           G_suppress_warnings (0);
           if (ier < 0)
	      { 
              Clear_info ();
	      sprintf(buffr," Do you want to create SUBJ/ file <%s>? ",N_subj_file);
              ans2 = curses_yes_no_default (2, buffr, 1);
              if (ans2)
	         {
                 pcats->title = G_store (N_subj_file);
                 pcats->num = 0;
                 pcats->list[0].num = 0;
	         title = "no data\0";
                 pcats->list[0].label = G_store(title);
                 pcats->count = 1;
                 pcats->fmt = NULL;
                 pcats->m1 = 0.0;
                 pcats->a1 = 0.0;
                 pcats->m2 = 0.0;
                 pcats->a2 = 0.0;
	         return(1);
	         }
              else break;
              }  /* for ier < 0 */
           else
	   {
	   return(1);
	   }
           }   /* end of while */
	 }    /* for if ans */
      if (ans == 0) return(0);
      }  /* end while */
}

ask_name(pcats)
    struct Categories *pcats ;
{
    int i, icode, recd;
    char buffr[128], area_name[40], cat_name[40];
    char *nptr, *cptr ;

    while (1)
      {
      Clear_info();
      Write_info( 4, "   Enter a label (<CR> to END): ");
      Get_curses_text (buffr,40) ;
      if (!strlen(buffr)) 
	 {
         Clear_info();
         return(0);
         }

      strcpy(area_name,buffr);
      nptr = area_name;
/*sprintf(buffr,"title=  %s, count= %d ",pcats->title,pcats->count);
Write_info(3,buffr);
sleep(2);*/

	/* find input string in category struct, assign category value to the
		    area_name based on category file record number*/
      recd = pcats->count;             /* set the number of categories */
      for (i=0;i<recd;i++)                /* category search */
	{		 
	                                    /* get a category label */
/*sprintf(buffr , "i= %d,  label= |%s|",i,pcats->list[i].label);
Write_info(3,buffr);*/
        sscanf (pcats->list[i].label, "%s", cat_name);
	cptr = cat_name;                /* first part only */

/*sprintf(buffr , "comp: cptr= |%s|, nptr= |%s|",cptr,nptr);
Write_info(4,buffr);
sleep(2);*/
	if (strncmp(nptr,cptr,strlen(cptr)) == 0)     /* compare for match */
	   {                           /* match, assigned already */
	   icode = pcats->list[i].num; /* set icode to category code */
	   return(icode);
	   }
	} 
	/* end of category search, NO category names match */

      Clear_info ();
      sprintf(buffr," Add new category <%d>, named <%s> ? ",recd,nptr);
      if (curses_yes_no_default (2, buffr, 1)) 
	 {                                      /* user said YES */
	 icode = pcats->count = recd++;         /* next category value */
	 G_set_cat ((CELL)icode, nptr, pcats);  /* create entry */
         G_sort_cats (pcats);
	 return(icode);
	 }
      }   /* end of while */
}

read_cats (element, name, mapset, pcats, full)
    char *element;
    char *name ;
    char *mapset ;
    struct Categories *pcats ;
{
    FILE *fd ;
    CELL cat;
    int old;
    long num;
    char buffr[128];


    if (!(fd = G_fopen_old (element, name, mapset)))
	return -2 ;

/* Read the number of categories */
    if (G_getl(buffr,sizeof buffr,fd) == NULL)
	goto error;

    if (sscanf ( buffr, "# %ld"   , &num) == 1)
	old = 0;
    else if (sscanf ( buffr, "%ld"   , &num) == 1)
	old = 1;
    else
	goto error;
    if (num < 0)
	goto error;
    if (!full)
    {
	fclose (fd);
	return (CELL) num;
    }

/* Read the title for the file */
    if (G_getl(buffr,sizeof buffr,fd) == NULL)
	goto error;
    G_strip (buffr);
    G_ascii_check(buffr) ;
    G_init_cats ((CELL)num, buffr, pcats);
    if (!old)
    {
	char fmt[256];
	float m1,a1,m2,a2;
	if (G_getl(fmt,sizeof fmt,fd) == NULL)
		goto error;
/* next line contains equation coefficients */
	if (G_getl(buffr,sizeof buffr,fd) == NULL)
		goto error;
	if(sscanf(buffr, "%f %f %f %f", &m1, &a1, &m2, &a2) != 4)
		goto error;
	G_set_cats_fmt (fmt, m1, a1, m2, a2, pcats);
    }

/* Read all category names */
    for (cat=0;;cat++) 
    {
	char label[256];
	if (G_getl(buffr, sizeof buffr, fd) == 0)
	    break;
	if (old)
	    G_set_cat (cat, buffr, pcats) ;
	else
	{
	    *label = 0;
	    if (sscanf (buffr, "%ld:%[^\n]", &num, label) < 1)
		goto error;
	    G_set_cat ((CELL)num, label, pcats);
	}
    }
    G_sort_cats (pcats);

    fclose (fd);
    return 0 ;
error:
    fclose (fd);
    return -1 ;
}

write_cats (element, name, pcats)
    char *element ;
    char *name ;
    struct Categories *pcats ;
{
    FILE *fd ;
    int i,n;


    if (!(fd = G_fopen_new (element, name)))
	return -1;

/* write # cats - note # indicate 3.0 or later */
    fprintf(fd,"# %ld categories\n", (long) pcats->num);

/* title */
    fprintf(fd,"%s\n", pcats->title!=NULL?pcats->title:"") ;

/* write format and coefficients */
    fprintf(fd,"%s\n", pcats->fmt!=NULL?pcats->fmt:"") ;
    fprintf(fd,"%.2f %.2f %.2f %.2f\n",
	    pcats->m1, pcats->a1, pcats->m2, pcats->a2) ;

/* write the cat numbers:label */
    G_sort_cats (pcats);
    n = pcats->count;
    for (i = 0; i < n; i++)
	fprintf(fd,"%ld:%s\n",
		(long) pcats->list[i].num,
		pcats->list[i].label!=NULL?pcats->list[i].label:"") ;
    fclose (fd) ;

    return(1) ;
}
