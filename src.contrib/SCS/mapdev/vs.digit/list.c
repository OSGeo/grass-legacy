/* @(#)list.c	1.0   04/90 */
/*-->   created for its purpose by RL Glenn, SCS
** modified by RL Glenn  12/1991
** USDA, SCS, Tech. Infor. Sys. Division
*/

#include "gis.h"
#include "digit.h"
#include "popup.h"

#ifdef SCS_MODS

list(ELEM)
char *ELEM;
{
    char path[1000];
    char buf[400], mapset[20];
    char line[80], *b;
    FILE *ls, *spath;
    FILE *popen();
    int count, line_num;

    count = 0;
    line_num = 1;

    sprintf(path,"%s/%s/SEARCH_PATH",G_location_path(),G_mapset());
    if (access(path, 0) == 0)
       {
       if (spath = fopen(path,"r"))
          {
	  while (fgets(buf, sizeof buf, spath))
             {
	         /*  remove the trailing newline */
	      for (b = buf; *b; b++)
	          if (*b == '\n')
	             *b = 0;

              sprintf(mapset,"%s", buf);
              G__file_name (path, ELEM, "", mapset);
              if(access(path, 0) == 0)
              {
	          sprintf(buf,"ls -C %s", path);

	          if (ls = popen(buf,"r"))
	          {
	              fprintf(stderr, "%s files available in <%s>:\n", ELEM,mapset);
	              while (fgets(buf, sizeof buf, ls))
	                 {
	                   /*  remove the trailing newline */
	                 for (b = buf; *b; b++)
	                    if (*b == '\n')
	                       *b = 0;

                         fprintf(stderr,"%s\n", buf);
	                 }
                      pclose (ls);
                  }
              }
          }
          fclose(spath);
       }
    fprintf(stderr,"   Hit <return> to continue");
    G_gets (buf,1) ;
    
    }
    else
       {
       fprintf(stderr,"   No SEARCH_PATH in this mapset\n");
       fprintf(stderr,"   Hit <return> to continue");
       G_gets (buf,1) ;

       }
    
    return ;
}
#else
static int xx;
#endif /* SCS_MODS */
