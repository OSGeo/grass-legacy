/* @(#)list.c	1.0   04/90 */
/*-->   created for its purpose by RL Glenn, SCS
 */

#include "gis.h"

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

    Clear_base();
    sprintf(path,"%s/%s/SEARCH_PATH",G_location_path(),G_mapset());
    if (access(path, 0) == 0)
       {
       if (spath = fopen(path,"r"))
          {
          Clear_info();
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
	              sprintf(line, "%s files available in <%s>:", ELEM,mapset);
	              Write_base(line_num++,line);
	              while (fgets(buf, sizeof buf, ls))
	                 {
	                   /*  remove the trailing newline */
	                 for (b = buf; *b; b++)
	                    if (*b == '\n')
	                       *b = 0;

                         sprintf(line,"%s", buf);
                         Write_base(line_num++,line);
	                 }
                  pclose (ls);
	          Write_base(line_num++, " ");
                  }
              }
          }
          fclose(spath);
       }
/*  Clear_info();
    Write_info( 4, "   Hit <return> to continue ");
    Get_curses_text (buf,1) ;
    Clear_base();
    Write_base_win(); */
    }
    else
       {
       Clear_info();
       Write_info( 3, "   No SEARCH_PATH in this mapset");
       Write_info( 4, "   Hit <return> to continue ");
       Get_curses_text (buf,1) ;
       }
    
    return ;
}
#else
static int xx;
#endif /* SCS_MODS */
