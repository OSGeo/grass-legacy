/* %W% %G% */
/* get_prj_ellp.c    1.0   6/182/91
*    Created by : R.L.Glenn , Soil Conservation Service, USDA
*    Purpose: function
*			Provide a couple of functions to get mapgen
*			projection and ellipse parameters.
*    Input arguements : empty string
*    Output arguements: projection parameters in string
*
*		Note: All functions are callable directly see
*			g.get_stp	g.get_fips	g.stp_proj  geo
*
*/
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>

#define E_MAX 40
#define P_MAX 100

struct Proj_List
{
	char *entry       ;   /* output from mapgen proj */
} *proj               ;


int get_ellp(strng)
char strng[];
{
	 int i,j, start, num, s_cnt, success=0;
         char *ptr, ans[10];
	 char buf1[200], buf2[80], buf3[50], *G_store();
	 FILE *sys;

	 sprintf(buf1,
		 "%s/mapgen/bin/proj +ellps=list | awk '{if (NR != 1) print $0}' | cut -c1-12,47-79", getenv("GISBASE"));

	 if (sys = popen(buf1,"r"))
	    {
		/* Allocate memory for Proj_List */
            proj = (struct Proj_List *) G_calloc ( E_MAX , sizeof(struct Proj_List));
	    s_cnt = -1;
	    while (fgets(buf2, sizeof(buf2), sys) != NULL)
	       {
               s_cnt++;
	       ptr = buf2;
	       while (*ptr == '\040') ptr++; /* remove leading spaces */
	       sprintf(buf3,"%s",ptr);
	       G_strip(buf3);
	       proj[s_cnt].entry = G_store(buf3);
	       }
            pclose (sys);
	    }

	 for(i=0; i<=s_cnt; i++)
	    {
	    for(;;)
	       {
	       start = 1;
	       for (j=i; j<=i+18 && j<=s_cnt; j++)
	          {
	          if (start)
	             {
		     G_clear_screen();
	             fprintf(stderr, "\n\n\tSPHEROIDS available :\n");
		     start = 0;
		     }
                  fprintf(stderr,"\t%d:\t%s",j+1, proj[j].entry);
	          }
               fprintf(stderr,"\n    Enter a spheroid number, or <CR> to continue : ");
	       gets(ans);
	       if (strlen(ans) == 0) break;  /* hit a CR */
	       sscanf(ans,"%[0-9]",buf2);
	       num = atoi(buf2) -1;
               if (num >= 0 && num <=s_cnt)
	          {
                  ptr = proj[num].entry;
		  while (*ptr != '\n')
                     {  /* find imbedded ':', set that to null */
		     if (*ptr == '\072') { *ptr = '\0'; break; }
		     ptr++;
                     }
                  sprintf(strng,"%s",proj[num].entry);
		  success = 1;
	          return(success);
		  }
	       fprintf(stderr,"\n\t\t  *** UNKNOWN spheroid *** \n");
	       sleep(2);
	       }
	    i = --j;
	    }
	 success = 0;
	 return(success);
}

int get_proj(strng)
char strng[];
{
	 int i,j, start, num, p_cnt, success=0;
         char *ptr, ans[10];
	 char buf1[200], buf2[80], buf3[50], *G_store();
	 FILE *sys;

	 sprintf(buf1,
		 "%s/mapgen/bin/proj +proj=list | awk '{if (NR != 1) print $0}' | cut -c9-79", getenv("GISBASE"));

	 if (sys = popen(buf1,"r"))
	    {
		/* Allocate memory for Proj_List */
            proj = (struct Proj_List *) G_calloc ( P_MAX , sizeof(struct Proj_List));
	    p_cnt = -1;
	    sprintf(buf3,"stp      -> State Plane\n");
		/* first entry of proj list is blank, 
		    put stp there - R.Glenn, SCS */
	    while (fgets(buf2, sizeof(buf2), sys) != NULL)
	       {
               p_cnt++;
	       if (p_cnt == 0)
		  {
		  proj[p_cnt].entry = G_store(buf3);
		  continue;
		  }
	       ptr = buf2;
	       while (*ptr == '\040') ptr++; /* remove leading spaces */
	       sprintf(buf3,"%s",ptr);
	       G_strip(buf3);
  	       proj[p_cnt].entry = G_store(buf3);
	       }
            pclose (sys);
	    }

	 for(i=0; i<=p_cnt; i++)
	    {
	    for(;;)
	       {
	       start = 1;
	       for (j=i; j<=i+18 && j<=p_cnt; j++)
	          {
	          if (start)
	             {
		     G_clear_screen();
		     fprintf(stderr, "\n\n\tPROJECTIONS available :\n");
		     start = 0;
		     }
                  fprintf(stderr,"\t%d:\t%s",j+1, proj[j].entry);
	          }
               fprintf(stderr,"\n    Enter a projection number, or <CR> to continue : ");
	       gets(ans);
	       if (strlen(ans) == 0) break;  /* hit a CR */
	       sscanf(ans,"%[0-9]",buf2);
	       num = atoi(buf2) - 1;
               if (num >= 0 && num <=p_cnt)
	          {
                  ptr = proj[num].entry;
		  while (*ptr != '\n')
                     {  /* find first whitespace, set it to null */
		     if (*ptr == '\040') { *ptr = '\0'; break; }
		     ptr++;
                     }
		  sprintf(strng,"%s",proj[num].entry);
		  success = 1;
	          return(success);
		  }
	       fprintf(stderr,"\n\t\t  *** UNKNOWN projection *** \n");
	       sleep(2);
	       }
	    i = --j;
	    }
	 success = 0;
	 return(success);
}
