/*  @(#)digitize.c    2.1  6/26/87  */
/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
/*  Last modified by R.L. Glenn  12/1991
**  USDA Tech. Infor. Sys. Division
*/

#include <stdio.h>
#include "gis.h"
#include "digit.h"
#include "dig_head.h"
#include "dig_defines.h"
#include "popup.h"

/* made these global so modes would remain set if they leave and return */
static int    type = AREA;
static int    mode = STREAM;
static int    hold = STREAM;
static int	auto_label = 0;

#ifdef SCS_MODS
static int d_get_names, ier, psu_site;
static char buff2[40];
#endif /* SCS_MODS */

Digitize ()
{
    int command;
    int Pass;
    int digit=1, background_color, text_color, menu_left, menu_top;
    int div_color, ret, chr;
    int help_for, help_ret, help_cnt;
    struct Categories lcats;
    char buff[60];
    static char *sav_opts[100];

    menu_left = Next_l + 1;
    menu_top = Next_t;

    mode = POINT;
    Pass = 0;
    d_get_names = 0;

    while(digit)
    {
    background_color = D_translate_color(BC_MAIN) ;
    text_color       = D_translate_color(TC_MAIN) ;
    div_color        = D_translate_color(DC_MAIN) ;

    options[0] = "  Digitize Menu\0";
    options[1] = "  ";
    options[2] = "Digitize";
    switch (type) {
	case AREA: options[3] = "Type >Area Edge";
		   break;
        case DOT:  options[3] = "Type >Site     ";
		   break;
        case PSU:  options[3] = "Type >PSU      ";
		   break;
        case LINE: options[3] = "Type >Line     ";
		   break;
		  }
    if (auto_label == 0)
        sprintf(buff,"Auto Label >Disabled ");
    else
        sprintf(buff,"Auto Label >%8d ", auto_label);
    options[4] = (char *) malloc (strlen (buff) + 1);
    sprintf(options[4],"%s", buff);

    options[5] = " ";
    options[6] = "Replot Screen",
    options[7] = "Zoom";
    options[8] = "Help";
    options[9] = "Return to Main menu";
    options[10] = '\0';
    Dchoose(MEN.name) ;

            ret = popup_menu(
                    background_color,
                    text_color,
                    div_color,
                    menu_top,
                    menu_left,
                    MEN_SIZE,
		    "digit",
		    _digit
                    ) ;
    if (_digit) _digit = 0;   /* don't save menu panel more than once */

	    switch (ret) {
		    case 2:
		        if (type != PSU)
		        {
/*DEBUG*/ debugf ("type %d  !=  PSU %d\n", type, PSU);
			    if (type - PSU)
			        debugf ("Type - PSU = %d(nonzero)\n", type - PSU);
			    else
			        debugf ("Type - PSU == 0\n");

			    do_digitize(CM, type == DOT ? POINT : mode, type, 
			        CM->prune_thresh, 1, 0);
		        }
		        else
		        {
/*DEBUG*/ debugf ("Calling do_psu_dig ()\n");
                           do_psu_dig(CM, mode, type, CM->prune_thresh);
                           auto_label = 0;
		        }

                        Dchoose(MEN.name) ;
		        break;
		    case 3:
		        switch (type) {
			    case LINE:
			        type = AREA;
/*--> make sure auto_label is OFF for area_edges , SCS, R Glenn */
                                auto_label = 0;
			        break;
			    case AREA:
			        type = DOT;
			        break;
			    case DOT:
			        type = PSU;
			        break;
			    case PSU:
			        type = LINE;
			        break;
		            }
		       break;
		     case 4:        /* Names NOT category codes */ 
			 if (type == DOT || type == LINE || type == PSU)
		         {
			     if (!d_get_names) d_get_names = ask_for_name(type,&lcats);
/*              	     G_clear_screen (); */
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

                                     sprintf(buff," PSU must be named, NOT numbered");
                                     message[0] = (char *) malloc (strlen (buff) + 1);
                                     sprintf(message[0],"%s", buff);
                                     message[1] = '\0';

                                     Dchoose(MEN.name) ;
                                     popup_messg( "warning", 1) ;

				     sleep (3);
                                     erase_popup("warning");
				     }
				     auto_label = 0;
			         }
			     }
		         }
                       else
			 {
	                 sprintf(buff, 
				  "Area edges CANNOT be auto_labeled") ;
                         message[0] = (char *) malloc (strlen (buff) + 1);
                         sprintf(message[0],"%s", buff);
                         message[1] = '\0';

                         popup_messg( "warning", 1) ;
                         sleep(2);
	                 erase_popup("warning");
			 }
		       break;

	            case 6:
                       Dchoose(DIG.name) ;
	               replot (CM);
                       Dchoose(MEN.name) ;
	               break;
	            case 7:
                       Dchoose(MEN.name) ;
	               zoom_window ();
		       erase_popup("zoom");
		       _zoom = 1;
	               break;
	            case 8:
		       for (help_cnt=0; help_cnt<=10; help_cnt++)
			  sav_opts[help_cnt] = options[help_cnt];
	               if (Help (&M_digit))
		       {
                       background_color = D_translate_color(BC_HELP) ;
                       text_color       = D_translate_color(TC_HELP) ;
                       div_color        = D_translate_color(DC_HELP) ;
		       help_for = 1;
                       while(help_for)
                          {
			  for (help_cnt=0; help_cnt<=10; help_cnt++)
			    options[help_cnt] = sav_opts[help_cnt];
		            options[0] = "   HELP for",
                            options[9] = "Exit from help menu";
                               help_ret =  popup_menu(
                                       background_color,
                                       text_color,
                                       div_color,
                                       menu_top,
                                       menu_left,
                                       MEN_SIZE,
				       "d_help",
				       1
                                       ) ;
	                    switch (help_ret) {
	                          case 2:
	                          case 3:
	                          case 4:
                                       Help_item(&M_digit,help_ret);
	                               break;
	                          case 6:
	                          case 7:
	                          case 8:
                                       Help_item(&M_main,help_ret+3);
	                               break;
	                          case 9:
	                               help_for = 0;
                                       background_color = D_translate_color(BC_MAIN) ;
                                       text_color       = D_translate_color(TC_MAIN) ;
                                       div_color        = D_translate_color(DC_MAIN) ;
				       erase_popup("d_help");
                                       erase_popup("info_help");
	                               break;
	                          default:
	                               break;
		                    }
                          }
                       }
	               break;
		    case 9:
		         Pass = digit = 0;
		         break;
		    default:
		         break;
	            }
    }
 
    return (Pass);
}


do_digitize(map, mode, type, sample_thresh, multi, close_area)
    struct Map_info *map;
    int mode;
    int type;
    double sample_thresh;
    int multi;   /* for SCS PSU */
    int close_area;	/* snap last point to first for SCS */
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
	/* if DOT, can use STOP digitizing to quit */

	if (type != DOT)
	{
	if (!mouse_yes_no ("Begin digitizing? "))
		    return (0);
	}
	else
        {
	    sprintf(buffer, "Site digitizing") ;
            message[0] = (char *) malloc (strlen (buffer) + 1);
            sprintf(message[0],"%s", buffer);
            message[1] = '\0';

            Dchoose(MEN.name) ;
            popup_messg( "info", 1) ;
            sleep(2);
	    erase_popup("info");
        }
        
	stream_mode = mouse_collect_points(mode, (char) type, &Xpoints);
	flush_keyboard ();

	if (close_area)
	{
      /* match the first and last points to snap */
#ifndef SCS_MODS
            dig_alloc_points (&Xpoints, Xpoints.n_points+1); 
#endif
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

	/*  toss out degenerate lines */
	if (type != DOT)
	{
	  if (!close_area)	/* SCS*/
	  {
	    if (Xpoints.n_points == 1 ||  /* are all points w/in snapping thresh? */
		 dig_is_line_degenerate (&Xpoints, map->head.map_thresh))
	    {
		Xpoints.n_points = 0;
		BEEP;

                sprintf(buffer,"Only 1 point digitized, Ignoring...");
                message[0] = (char *) malloc (strlen (buffer) + 1);
                sprintf(message[0],"%s", buffer);
                message[1] = '\0';

                Dchoose(MEN.name) ;
                popup_messg( "warning", 1) ;

		sleep (3);
                erase_popup("warning");
		goto d_done;
	    }
	  }
	}
	else
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
	if (type != DOT)
	{
	  if (node.cnt)
	  {
	    if (Beep_On)
		BEEP;   /* Beep for at least one node */
	    fflush(stdout);
	    sprintf(buffer, "NOTE: %d new nodes needed", node.cnt);
            message[0] = (char *) malloc (strlen (buffer) + 1);
            sprintf(message[0],"%s", buffer);
            message[1] = '\0';
 
            Dchoose(MEN.name) ;
            popup_messg( "info", 1) ;

	    if(node.cnt == 2)     /* Beep for second node */
	    {
		sleep(1);
		if (Beep_On)
		    BEEP;
	    }
	    sleep(2);
	    erase_popup("info");
	  }
	  else
	  {
	    sprintf(buffer, "NOTE: Zero new nodes needed");
            message[0] = (char *) malloc (strlen (buffer) + 1);
            sprintf(message[0],"%s", buffer);
            message[1] = '\0';

            Dchoose(MEN.name) ;
            popup_messg( "info", 1) ;
            sleep(2);
            erase_popup("info");
	  }
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

	      yes_no = mouse_yes_no(str) ;
	}
	if (type == DOT)
		erase_popup("show_stat");

	if (yes_no)
	{
	    if (do_graphics ())
		erase_line ((unsigned char)type, &Xpoints, 0, NULL);

	    Changes_Made = 1;

	    line = new_line (map, (unsigned char) type, &node, &Xpoints);
	    if (line < 0)
	    {
		BEEP;

                sprintf(buffer,"Error creating new line.");
                message[0] = (char *) malloc (strlen (buffer) + 1);
                sprintf(message[0],"%s", buffer);
                message[1] = '\0';

                Dchoose(MEN.name) ;
                popup_messg( "warning", 1) ;

		sleep (4);
                erase_popup("warning");
		return (-1);
	    }
	    if (do_graphics())
		display_line ((unsigned char) type, &Xpoints, line, map);

	    /* is this an area boundary that will affect neighbor areas? */
	    if (type == AREA)
	    {
		if (area = check_next (map, line, RIGHT))
		    Del_area (map, area);
		if (area = check_next (map, line, LEFT))
		    Del_area (map, area);
		if (area = check_next (map, -line, RIGHT))
		    Del_area (map, area);
		if (area = check_next (map, -line, LEFT))
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
            return(0);
	}
d_done:
/*      G_clear_screen(); */
	;

    }	/*  while(1)  */

    /*NOTREACHED*/

}	/*  do_digitize()  */

#ifdef SCS_MODS
do_psu_dig(map, mode, type, sample_thresh)
    struct Map_info *map;
    int mode;
    int type;
    double sample_thresh;
{
    char buffer[64];
    int button;
    int yes_no, status ;
    int site_no, count;

    type = AREA;
    status = do_digitize(CM, mode, type, CM->prune_thresh, 0, 1);
    if (status && auto_label != 0)
       {
       label_psu(map,auto_label);
/*     G_clear_screen(); */
       }
    type = DOT;
    sprintf(buffer,"Digitize sites ? ");
    yes_no = mouse_yes_no(buffer) ;

    count = 1;
    if (yes_no)
      while (count <= 3)
      {
/*      G_clear_screen (); */
        { 
        sprintf(buffer,"Digitize site # %d ? ",count);
	yes_no = mouse_yes_no(buffer) ;

        if (yes_no) 
	    auto_label = psu_site + count;
        else
	{
	    count++;
	    continue;
	}

        status = do_digitize(CM, mode, type, CM->prune_thresh, 0, 0);
        if (status > 0) count++;
        }
      }
    return(0) ;
}	/*  do_psu_dig()  */

get_site_num(area_num, pcats)
    int area_num;
    struct Categories *pcats ;
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
