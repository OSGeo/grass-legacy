/*  @(#)head_info.c	2.1  6/26/87  */
/*
**  Modified Mar 1992 Ron Glenn, SCS
**     to set up windows on vector file 
*/
#include "gis.h"
#include "dig_head.h"
#include "digit.h"
#include <stdio.h>

get_head_info(have_old, dhead)
	int  have_old ;
	struct dig_head *dhead;
{
    int repeat = 1, reset=0;
    char answer[5], buf[100];
    struct Cell_head Window, temp_wind;

#ifdef SCS_MODS
    if( ! have_old)
	    strcpy(dhead->organization, "USDA, SCS") ;
#else
    if( ! have_old)
	    strcpy(dhead->organization, "US Army Const. Eng. Rsch. Lab") ;
#endif
    
    if (got_region)  /* set window from named region */
	  {
	  if (G__get_window (&Window, "windows", N_region, N_region_mapset) < 0)
            {
	    sprintf(buf,"can't read region <%s> in <%s>",
		  N_region, N_region_mapset);
            G_fatal_error(buf);
	    }
          reset = 1;
          }
    else if (Disp_backdrop) /* set window from backdrop raster */
	G_get_cellhd (N_backdrop, N_backdrop_mapset, &Window);
    else if (Disp_overlay)  /* set window from overlay file */
	  {
	  struct Map_info Vmap;
	  Vect_set_open_level(1);
	  if (1 != Vect_open_old (&Vmap, N_overlay, N_overlay_mapset))
	    {
	    sprintf(buf,"can't open overlay file <%s> in <%s>",
		N_overlay, N_overlay_mapset);
            G_fatal_error(buf);
	    }
          Window.north = Vmap.head.N;
	  Window.south = Vmap.head.S;
	  Window.west  = Vmap.head.W;
	  Window.east  = Vmap.head.E;
	  Window.zone  = Vmap.head.plani_zone;
	  Vect_close(&Vmap);
	  }
    else       /* set window from current window */
	{
        if (G__get_window (&Window, "", "WIND", G_mapset()) < 0)
	   {      /* current window failed, use default window */
	   G_get_default_window (&Window);
	   G_put_window (&Window);
	   }
	}

    while (repeat)
    {
    V_clear() ;
    V_line(1,"Provide the following information:") ;

    V_line(3,"Your organization") ;
    V_line(4,"Todays date (mon,yr)") ;
    V_line(5,"Your name") ;
    V_line(6,"Map's name") ;
    V_line(7,"Map's date") ;
    V_line(8,"Map's scale         1:") ;
    V_line(9,"Other info") ;
    V_line(10,"Zone") ;
    V_line(11,"West edge of area") ;
    V_line(12,"South edge of area") ;
    V_line(13,"East edge of area") ;
    V_line(14,"North edge of area") ;

    V_ques( dhead->organization, 's', 3,  20, 30-1) ;
    V_ques( dhead->date,         's', 4,  20, 20-1) ;
    V_ques( dhead->your_name,    's', 5,  20, 20-1) ;
    V_ques( dhead->map_name,     's', 6,  20, 41-1) ;
    V_ques( dhead->source_date,  's', 7,  20, 11-1) ;
    V_ques( &dhead->orig_scale,  'i', 8,  22, 9) ;
    V_ques( dhead->line_3,       's', 9,  20, 59-1) ;

    /* set up default window */
    if (!have_old)
    {
	dhead->W = Window.west;
	dhead->S = Window.south;
	dhead->N = Window.north;
	dhead->E = Window.east;
	dhead->plani_zone = Window.zone;
	dhead->orig_scale = 1;

#ifdef NO_PORTABLE
        dhead->portable = 0;
#else
        dhead->portable = 1;
#endif

    }
    if (reset)
    {
	if (got_region)
	   {
	   dhead->W = Window.west;
	   dhead->E = Window.east;
	   dhead->N = Window.north;
	   dhead->S = Window.south;
	   }
        else
	   {
	   if (dhead->W > Window.west) dhead->W = Window.west;
	   if (dhead->E < Window.east) dhead->E = Window.east;
	   if (dhead->N < Window.north) dhead->N = Window.north;
	   if (dhead->S > Window.south) dhead->S = Window.south;
	   }
    }

    V_ques( &dhead->plani_zone,  'i', 10, 20, 5)  ;
    V_ques( &dhead->W,           'd', 11, 20, 14) ;
    V_ques( &dhead->S,           'd', 12, 20, 14) ;
    V_ques( &dhead->E,           'd', 13, 20, 14) ;
    V_ques( &dhead->N,           'd', 14, 20, 14) ;
    

    V_call() ;

/*   Check for error flag */
    if (dhead->orig_scale <= 0)
       {
       fprintf(stderr,"\nYou must have a scale value\n");
       fprintf(stderr,"\n\t\tHit RETURN -->");
       gets(answer);
       }
    else if (Disp_backdrop && !got_region)
       { /*   Check for raster beyond vector boundaries */
       if (dhead->W > Window.west  ||
           dhead->E < Window.east ||
           dhead->N < Window.north ||
           dhead->S > Window.south)
	     {
             fprintf(stderr,"\nThe current N,S,E,W  will NOT show ALL backdrop raster data\n");
	     fprintf(stderr,"\nDo you want these values changed to include ALL raster data [n] ?");
	     G_gets(answer);
	     if (*answer == 'y' || *answer == 'Y') reset = 1;
	     else repeat = 0;
             }
       else repeat = 0;
       }
    else if (Disp_overlay && !got_region)
       { /*   Check for overlay beyond vector boundaries */
       if (dhead->W > Window.west  ||
           dhead->E < Window.east ||
           dhead->N < Window.north ||
           dhead->S > Window.south)
	     {
             fprintf(stderr,"\nThe current N,S,E,W  will NOT show ALL overlay vector data\n");
	     fprintf(stderr,"\nDo you want these values changed to include ALL overlay data [n] ?");
	     G_gets(answer);
	     if (*answer == 'y' || *answer == 'Y') reset = 1;
	     else repeat = 0;
             }
       else repeat = 0;
       }
    else repeat = 0;
    }
}
