/*  @(#)digitize.c    2.1  6/26/87  */
/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include <stdio.h>
#include <unistd.h>
#include "gis.h"
#include "digit.h"
#include "dig_curses.h"
#include "display_line.h"
#include "Map_proto.h"
#include "debug.h"
#include "line_pnts.h"
#include "keyboard.h"
#include "local_proto.h"

/* made these global so modes would remain set if they leave and return */
static int    type = AREA;
static int    mode = STREAM;
static int    hold = STREAM;
static int	auto_label = 0;

#ifdef SCS_MODS
static int d_get_names, ier, psu_site;
static char buff2[40];
#endif /* SCS_MODS */

int Digitize (void)
{
    int command;
    int Pass;
    int ret;
    int chr;
    int catn;
    int ans = 0;
    struct Categories lcats;


    /* if mouse digitizing, STREAM mode is disabled */
    if (Digtiz_Device == MOUSE)
	mode = POINT;


    Pass = 0;
    Set_G_Mask (MG_DIGIT, OFF);

    while(1) 
    {
	_Clear_info ();
	update_global_menu ();
	_Write_dig_win();
	_Write_type_info();
	_show_mode(mode, type, auto_label);
	_Base_refresh ();

	if ((command = get_menu_command (&M_digit, &chr)) > 0)
	{
	    switch(command)
	    {
		case MDC_UNDO:
		    break;
		case MDC_REPLOT:
		    Replot_screen ();
		    _Write_type_info();
		    show_mode(mode, type, auto_label);
		    break;
		case MDC_QUIT:
		    goto DIGIT_END;
		    break;
		case MDC_MODE:
		    TOGGLE (mode);
		    show_mode(mode, type, auto_label);
		    break;
		case MDC_TYPE:
		    switch (type) {
			case LINE:
			    type = AREA;
			    break;
			case AREA:
			    type = DOT;
			    break;
#ifndef SCS_MODS 
			case DOT:
			    type = LINE;
			    break;
#else
			case DOT:
			    type = PSU;
			    break;
			case PSU:
			    type = LINE;
			    break;
#endif /* SCS_MODS */
		    }
/*DEBUG*/ debugf ("TYPE = %d\n", type);
		    show_mode(type == DOT ? POINT : mode, type, auto_label);
		    break;
		case MDC_LABEL:
#ifndef SCS_MODS
		    if(Cat_name)
		    {
			G_free(Cat_name);
			Cat_name = NULL;
		    }
		    ans = ask_for_name(type, &lcats);
		    auto_label = ask_cat ();
		    if (auto_label && ans)
		    {
			catn = ask_name(auto_label, &lcats);
			auto_label = (catn ? catn : auto_label);
		    }

		    show_mode(mode, type, auto_label);
		    break;
#else
                                             /* Names NOT category codes */
                    if (type == DOT || type == LINE || type == PSU)
		    {
			if (!d_get_names) d_get_names = ask_for_name(type,&lcats);
			Clear_info ();
			if (d_get_names > 0)
			{
			    auto_label = ask_name(&lcats);
			    if (type == PSU && auto_label)
				psu_site = get_site_num(auto_label, &lcats) - 1;
			}
			else
			{
			    if (type != PSU) auto_label = ask_cat();
			    else
			    {
				if (d_get_names = 0)
				{
				    sprintf(buff2,
				       " PSU must be named, NOT numbered");
				    Write_info (3,buff2); 
				    sleep (3);
				}
				auto_label = 0;
			    }
			}
		    }
		    break;

#endif /* SCS_MODS */
		case MDC_DIGIT:

/*DEBUG*/ debugf ("MDC_DIGIT:  TYPE = %d\n", type);

#ifndef SCS_MODS 
		    do_digitize(CMap, type == DOT ? POINT : mode, type, 
			CMap->prune_thresh, 1, 0);

#else	/* SCS_MODS */

		    if (type != PSU)
		    {
/*DEBUG*/ debugf ("type %d  !=  PSU %d\n", type, PSU);
			if (type - PSU)
			    debugf ("Type - PSU = %d(nonzero)\n", type - PSU);
			else
			    debugf ("Type - PSU == 0\n");

			do_digitize(CMap, type == DOT ? POINT : mode, type, 
			    CMap->prune_thresh, 1, 0);
		    }
		    else
		    {
/*DEBUG*/ debugf ("Calling do_psu_dig ()\n");
                       do_psu_dig(CMap, mode, type, CMap->prune_thresh);
                       auto_label = 0;
		    }
#endif /* SCS_MODS */

		    _Write_type_info();
		    show_mode(mode, type, auto_label);
		    break;
		default:
		    break;
	    }
	}
	else
	{
	    if ((ret = global_menu (chr, &M_digit)) > 0)
	    {
		Pass = ret;
		break;  /* return and execute new command */
	    }
	    if (ret < 0)
		BEEP;
	}
    }
DIGIT_END:
    
    Set_G_Mask (MG_DIGIT, ON);
    return (Pass);
}


int do_digitize (
    struct Map_info *map,
    int mode,
    int type,
    double sample_thresh,
    int multi,   /* for SCS PSU */
    int close_area	/* snap last point to first for SCS */
)
{
    char buffer[64];
    int stream_mode;
    struct new_node node;
    int line;
    int area;
    int yes_no ;
    static struct line_pnts Xpoints;
    static int first = 1;



    if (first)
    {
	first = 0;
	Xpoints.alloc_points = 0;
	Xpoints.n_points = 0;
    }

    while(1)
    {
	Dig_menu_opts ();
	Clear_info();	 /*New*/

	/* if DOT, can use STOP digitizing to quit 
	*/
	if (type != DOT)
	{
	    if (Digtiz_Device == MOUSE)
	    {
		while((yes_no = mouse_yes_no_zoom ("Begin digitizing? ", 0, NULL)) != 1)
			if(yes_no == 3)
		    		return (0);
	    }
	    else
#ifdef CURSORKEYS
	    if (D_cursor_buttons())
#endif
	    {
		if ( ! ask_driver_yes_no("Begin digitizing? ") )
			return(0) ;
	    }
	}
/*
	else
        {
 	    _Clear_base () ;
	    Write_base(10, "Site digitizing") ;
        }
*/

recollect:
	if (Digtiz_Device == MOUSE)
	    stream_mode = mouse_collect_points(mode, (char) type, &Xpoints);
	else
	    stream_mode = Collect_points(mode, (char) type, &Xpoints);
	flush_keyboard ();

	if (close_area)
	{
      /* match the first and last points to snap */
	    dig_alloc_points (&Xpoints, Xpoints.n_points+1);
	    Xpoints.x[Xpoints.n_points] = Xpoints.x[0];
	    Xpoints.y[Xpoints.n_points] = Xpoints.y[0];
	    Xpoints.n_points++;
	}

	/* 
	** requested to stop digitizing
	*/
	if (DOT && Xpoints.n_points == 0)
	    return (0);

    /*DEBUG*/ debugf ("Collected %d points \n", Xpoints.n_points);

	if (stream_mode)
	{
	    Xpoints.n_points = dig_prune(&Xpoints, sample_thresh);
    /*DEBUG*/ debugf ( "after prune:  %d points\n", Xpoints.n_points);
	}

	if(type == DOT)
	{
	    if (Xpoints.n_points == 1)
	    {
		dig_alloc_points (&Xpoints, 2);
		Xpoints.x[1] = Xpoints.x[0];
		Xpoints.y[1] = Xpoints.y[0];
		Xpoints.n_points = 2;
	    }
	}

	/* sites do not have nodes */
	{
/*DEBUG*/  debugf (" Entering Checknodes HAVE %d Points.\n", Xpoints.n_points);
	    dig_check_nodes (map, &node, &Xpoints);
/*DEBUG*/ debugf ( "Check_nodes returns  N1 = %d  N2 = %d\n", node.N1, node.N2);
	}
	if (node.cnt)
	{
	    if (Beep_On)
		BEEP;   /* Beep for at least one node */
	    fflush(stdout);
	    sprintf(buffer, "NOTE: %d new nodes needed", node.cnt);
	    Write_info(3, buffer);
	    if(node.cnt == 2)     /* Beep for second node */
	    {
		sleep(1);
		if (Beep_On)
		    BEEP;
	    }
	}
	else
	{
	    Write_info (3, "NOTE: Zero new nodes needed");
	}

	if (do_graphics())
	    highlight_line ((unsigned char) type, &Xpoints, 0, NULL);
	{
	  char *str;

	  switch (type) {
	      case DOT:
		  str = "Do you accept this site? ";
		  break;
	      case AREA:
		  if (close_area)
		      str = "Do you accept this area? ";
		  else
		      str = "Do you accept this area line? ";
		  break;
	      case LINE:
	      default:
		  str = "Do you accept this line? ";
		  break;
	  }

	  if (Digtiz_Device == MOUSE){
	      yes_no = mouse_yes_no_zoom(str, (unsigned char) type, &Xpoints) ;
	      if(yes_no == 2)
		      goto recollect;
	      yes_no = (yes_no == 1);
	  }else
	      yes_no = ask_yes_no(str) ;
	}

	if (yes_no)
	{
	    /*  toss out degenerate lines */
	    if (type != DOT)
	    {
	      if (!close_area)	/* SCS*/
	      {
	        if (Xpoints.n_points == 1 ||  /* are all points w/in snapping thresh? */
		     dig_is_line_degenerate (&Xpoints, map->head.map_thresh))
	        {
	    	    if (do_graphics())
			erase_line ((unsigned char) type, &Xpoints, 0, NULL);
		    Xpoints.n_points = 0;
		    BEEP;
		    Write_info(3, "Only 1 point digitized, Ignoring...");
		    sleep (3);
		    goto d_done;
	        }
	      }
	    }

	    if (do_graphics ())
		erase_line ((unsigned char)type, &Xpoints, 0, NULL);

	    Changes_Made = 1;
/*DEBUG*/ debugf ("entering new_line");
	    line = new_line (map, (unsigned char) type, &node, &Xpoints);
	    if (line < 0)
	    {
		BEEP;
		Write_info (2, "Error creating new line.");
		sleep (4);
		return (-1);
	    }
	    if (do_graphics())
		display_line ((unsigned char) type, &Xpoints, line, map);

	    /* is this an area boundary that will affect neighbor areas? */
	    if (type == AREA)
	    {
		if ((area = check_next (map, line, RIGHT)))
		    Del_area (map, area);
		if ((area = check_next (map, line, LEFT)))
		    Del_area (map, area);
		if ((area = check_next (map, -line, RIGHT)))
		    Del_area (map, area);
		if ((area = check_next (map, -line, LEFT)))
		    Del_area (map, area);
		/*
		** PSU, dont want digizing loop
		*/
                if (!multi) return(1); /* SCS_MODS */
	    }

	    if (auto_label)
	    {
		label_line (map, line, auto_label, &Xpoints);
                if (!multi) return(1); /* SCS_MODS */
	    }

	}
	else
	{
	    if (do_graphics())
		erase_line ((unsigned char) type, &Xpoints, 0, NULL);
	}
 if (Auto_Smooth)
      { angle_in_line (map,line); }
d_done:
	Clear_info();
	Xpoints.n_points = 0;
#ifdef CURSORKEYS
	/*  if they don't have buttons force them to start over */
	if (Digtiz_Device != MOUSE)
	    if ( ! D_cursor_buttons())
		return(0) ;
#endif
    }	/*  while(1)  */

    /*NOTREACHED*/

}	/*  do_digitize()  */

#ifdef SCS_MODS
int do_psu_dig (struct Map_info *map, int mode, int type, double sample_thresh)
{
    char buffer[64];
    int button;
    int yes_no, status ;
    int site_no, count;

    type = AREA;
    status = do_digitize(CMap, mode, type, CMap->prune_thresh, 0, 1);
    if (status && auto_label != 0)
       {
       label_psu(map,auto_label);
       Clear_base();
       }
    type = DOT;
    sprintf(buffer,"Digitize sites ? ");
    if (Digtiz_Device == MOUSE)
	yes_no = mouse_yes_no_zoom(buffer, type, NULL) ;
    else
	yes_no = ask_yes_no(buffer) ;

    count = 1;
    if (yes_no)
      while (count <= 3)
      {
        Clear_info ();
        { 
        sprintf(buffer,"Digitize site # %d ? ",count);
        if (Digtiz_Device == MOUSE)
	  yes_no = mouse_yes_no_zoom(buffer, type, NULL) ;
        else
	  yes_no = ask_yes_no(buffer) ;

        if (yes_no) 
	    auto_label = psu_site + count;
        else
	{
	    count++;
	    continue;
	}

        status = do_digitize(CMap, mode, type, CMap->prune_thresh, 0, 0);
        if (status > 0) count++;
        }
      }
    return(0) ;
}	/*  do_psu_dig()  */

int get_site_num (int area_num, struct Categories *pcats)
{
    int i, icode, recd;
    char buffr[128], area_name[40], cat_name[40];
    char *nptr, *cptr ;

      sprintf(area_name,pcats->list[area_num].label);
      nptr = area_name;

	/* find input string in category struct, assign category value to the
		    area_name based on category file record number*/
      recd = pcats->count;             /* set the number of categories */
      for (i=area_num+1;i<recd;i++)                /* category search */
	{		 
	                                    /* get a category label */
        sscanf (pcats->list[i].label, "%s", cat_name);
	cptr = cat_name;                /* first part only */

	if (strncmp(nptr,cptr,strlen(nptr)) == 0)     /* compare for match */
	   {                           /* match, return first found */
	   icode = pcats->list[i].num; /* set icode to category code */
	   return(icode);
	   }
	} 
	/* end of category search, NO category names match */
}
#endif /* SCS_MODS */
