/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
/*  Last modified by R.L. Glenn  12/1991
**  USDA Tech. Infor. Sys. Division
*/

#include "gis.h"
#include "digit.h"
#include "dig_head.h"
#include <stdio.h>
#include "dig_defines.h"
#include "popup.h"

int tell_area_label ();
int tell_area_unlabel ();
int make_area_label ();
int tell_line_label();	/* function called by find_line_with_mouse() */

static double local_x;	/* filled by label_area  */
static double local_y;	/* used by tell_area_label */
static int local_area;
static int local_prev;
static P_AREA local_struct;


int
Label()
{
    int command;		/* command user enters */
    int ret, subret;		/* get return values from subrs */
    int Pass;			/* Holds value to return to caller */
    int chr;
    int cat;
    int background_color, text_color, div_color;
    int menu_left, menu_top, sub_left, sub_top;
    int label=1, do_lab, un_lab;
    int help_for, help_ret, help_cnt;
    int ans=0, ier;
    struct Categories cats;
    static char *sav_opts[100];
    char messag[72], buf[100];

    menu_left = Next_l + 1;
    menu_top = Next_t;

    while (label)
    {
    background_color = D_translate_color(BC_MAIN) ;
    text_color       = D_translate_color(TC_MAIN) ;
    div_color        = D_translate_color(DC_MAIN) ;
    
    options[0] = "  Label Menu";
    options[1] = "";
    options[2] = "Label";
    options[3] = "UN-label";
    options[4] = "Bulk Label";
    options[5] = "Highlight Lines of category #";
    options[6] = "Display Areas of category #";
    sprintf(buf,"Contour interval:   %6d ",Contour_Interval);
    options[7] = (char *) malloc (strlen (buf) + 1);
    sprintf(options[7],"%s", buf);
    options[8] = "";
    options[9] = "Replot Screen",
    options[10] = "Zoom";
    options[11] = "Help";
    options[12] = "Return to Main menu";
    options[13] = '\0' ;

    if (Auto_Number)
        {
        /* read category file , if it exists*/
        G_suppress_warnings (1);
        ier = G__read_cats ("dig_cats", N_name, G_mapset(), &cats, 1);
        G_suppress_warnings (0);
        if (ier < 0)
	    Auto_Number = 0;
        } 

    ret = popup_menu(
                    background_color,
                    text_color,
                    div_color,
                    menu_top,
                    menu_left,
                    MEN_SIZE,
		    "label",
		    _label
                    ) ;

    if (_label) _label = 0;    /* don't save menu panel more than once */

	    switch (ret) {
	            case 2:  /* label */
			for (ier=0; ier<=9; ier++)
			  sav_opts[ier] = options[ier];
                        sub_top = Next_t;
			sub_left = Next_l + 1;
                        background_color = D_translate_color(BC_MAIN) ;
                        text_color       = D_translate_color(TC_MAIN) ;
                        div_color        = D_translate_color(DC_MAIN) ;
			do_lab = 1;

			while(do_lab)
			{
                        options[0] = "  Label What\0";
                        options[1] = "";
                        options[2] = "Areas";
                        options[3] = "Lines";
                        options[4] = "Sites";
                        options[5] = "Multiple Lines";
                        options[6] = "Contours ";
                        options[7] = "";
                        options[8] = "Return to Label Menu";
                        options[9] = '\0' ;

                        subret = popup_menu(
                                background_color,
                                text_color,
                                div_color,
                                sub_top,
                                sub_left,
                                MEN_SIZE,
				"sub_opt",
				1
                                ) ;
 
	                switch (subret) {
	                    case 2:  /* label areas */
                                  if (Auto_Number)
                                    {
                                    while(1)
		                     {         /* get numbers */
		                     cat = get_cat(&cats);
		                     if (!cat) break;
                                     label_area (CM, cat);
		                     }
                                    Dchoose(MEN.name) ;
				    }
				  else
				    {
                                    ans = ask_for_name(AREA, &cats);
		                      while (1)
		                        {
                                        if (ans) cat = ask_name(&cats);
                                        else cat = ask_cat();
		                        if (!cat) break;
		                        label_area (CM, cat);
		                        }
                                    Dchoose(MEN.name) ;
				    }
		                  break;
	                    case 3:  /* label lines */
                                  if (Auto_Number)
                                    {
                                    while(1)
		                     {         /* get numbers */
		                     cat = get_cat(&cats);
		                     if (!cat) break;
                                     label_lines (CM, cat);
		                     }
                                    Dchoose(MEN.name) ;
				    }
				  else
				    {
                                    ans = ask_for_name(LINE, &cats);
		                      while (1)
		                        {
                                        if (ans) cat = ask_name(&cats);
                                        else cat = ask_cat();
		                        if (!cat) break;
		                        label_lines (CM, cat);
		                        }
                                    Dchoose(MEN.name) ;
				    }
		                  break;
	                    case 4:  /* label sites */
                                  if (Auto_Number)
                                    {
                                    while(1)
		                     {         /* get numbers */
		                     cat = get_cat(&cats);
		                     if (!cat) break;
                                     label_sites (CM, cat);
		                     }
                                    Dchoose(MEN.name) ;
				    }
				  else
				    {
                                    ans = ask_for_name(DOT, &cats);
		                      while (1)
		                        {
                                        if (ans) cat = ask_name(&cats);
                                        else cat = ask_cat();
		                        if (!cat) break;
		                        label_sites (CM, cat);
		                        }
                                    Dchoose(MEN.name) ;
				    }
		                  break;
	                    case 5: /* label multiple lines */

                                  if (Auto_Number)
                                    {
                                    while(1)
		                     {         /* get numbers */
		                     cat = get_cat(&cats);
		                     if (!cat) break;
                                     label_mlines (CM, cat);
		                     }
                                    Dchoose(MEN.name) ;
				    }
				  else
				    {
                                    ans = ask_for_name(LINE, &cats);
		                      while (1)
		                        {
                                        if (ans) cat = ask_name(&cats);
                                        else cat = ask_cat();
		                        if (!cat) break;
		                        label_mlines (CM, cat);
		                        }
                                    Dchoose(MEN.name) ;
				    }
		                  break;
	                    case 6: /* label contour lines w/ others */
		                  while (1)
		                      if (0 == label_contour (CM, Contour_Interval))
			                  break;
                                  Dchoose(MEN.name) ;
		                  break;
	                    case 8: /* return label menu */
                                  background_color = D_translate_color(BC_MAIN) ;
                                  text_color       = D_translate_color(TC_MAIN) ;
                                  div_color        = D_translate_color(DC_MAIN) ;
				  do_lab = 0;
				  erase_popup("sub_opt");
			          for (ier=0; ier<=9; ier++)
			               options[ier] = sav_opts[ier];
		                  break;
	                    default: 
		                  break;
                            }
			}
		        break;

	            case 3:  /* UN-label */
			for (ier=0; ier<=8; ier++)
			  sav_opts[ier] = options[ier];
                        sub_top = Next_t;
			sub_left = Next_l + 1;
                        background_color = D_translate_color(BC_MAIN) ;
                        text_color       = D_translate_color(TC_MAIN) ;
                        div_color        = D_translate_color(DC_MAIN) ;
			un_lab = 1;

			while(un_lab)
			{
                        options[0] = "  UN-label What\0";
                        options[1] = "";
                        options[2] = "Areas";
                        options[3] = "Lines";
                        options[4] = "Sites";
                        options[5] = "Multiple Lines";
                        options[6] = "";
                        options[7] = "Return to Label Menu";
                        options[8] = '\0' ;

                        subret = popup_menu(
                                background_color,
                                text_color,
                                div_color,
                                sub_top,
                                sub_left,
                                MEN_SIZE,
				"sub_opt",
				1
                                ) ;
 
	                switch (subret) {
	                    case 2:  /* un-label areas */
		                  unlabel_area (CM);
                                  Dchoose(MEN.name) ;
		                  break;
	                    case 3:  /* un-label lines */
		                  unlabel_lines (CM);
                                  Dchoose(MEN.name) ;
		                  break;
	                    case 4:  /* un-label sites */
		                  unlabel_sites (CM);
                                  Dchoose(MEN.name) ;
		                  break;
	                    case 5:  /* un-label mult-lines */
		                  label_mlines (CM, 0);
                                  Dchoose(MEN.name) ;
		                  break;
	                    case 7: /* return label menu */
                                  background_color = D_translate_color(BC_MAIN) ;
                                  text_color       = D_translate_color(TC_MAIN) ;
                                  div_color        = D_translate_color(DC_MAIN) ;
				  un_lab = 0;
			          for (ier=0; ier<=8; ier++)
			            options[ier] = sav_opts[ier];
				  erase_popup("sub_opt");
		                  break;
	                    default: 
		                  break;
                            }
			}
		        break;

	            case 4:  /* bulk label */
                            if (!mouse_yes_no ( "This function will label EVERY   \nunlabeled line. Proceed?"))
				      {
		                      break;
				      }
		            cat = ask_cat ();
		            if (!cat) break;
		            label_all_lines (CM, cat);
                            Dchoose(MEN.name) ;
		            break;

	            case 5:  /* Highlight Lines of category # */
		            display_llines (CM);
                            Dchoose(MEN.name) ;
		            break;

	            case 6:  /* Display Areas of category */
		            display_lareas (CM);
                            Dchoose(MEN.name) ;
		            break;

	            case 7:  /* Contour interval */
		            {
		                int val;

                                sprintf (buf,
				   "   Enter New Contour Interval[5]   ") ;
                                message[0] = (char *) malloc (strlen (buf) + 1);
                                sprintf(message[0],"%s", buf);
                                message[1] = " ";
                                message[2] = " ";
                                message[3] = '\0';

                                Dchoose(MEN.name) ;
                                popup_messg( "info", 1) ;
                                popup_ques(20,&buf[0]);
				erase_popup("info");
		                val = atoi (buf);
		                if (!val)
			            {
		                    Contour_Interval = 5;
			            break;
			            }
		                Contour_Interval = val;
		            }
		            break;

	            case 9:
                       Dchoose(DIG.name) ;
	               clear_window ();
	               replot (CM);
                       Dchoose(MEN.name) ;
	               break;
	            case 10:
                       Dchoose(MEN.name) ;
	               zoom_window ();
		       erase_popup("zoom");
		       _zoom = 1;
	               break;
	            case 11:
		       for (help_cnt=0; help_cnt<=13; help_cnt++)
			  sav_opts[help_cnt] = options[help_cnt];
	               if (Help (&M_label))
		       {
                       background_color = D_translate_color(BC_HELP) ;
                       text_color       = D_translate_color(TC_HELP) ;
                       div_color        = D_translate_color(DC_HELP) ;
		       help_for = 1;
                       while(help_for)
                          {
			  for (help_cnt=0; help_cnt<=13; help_cnt++)
			    options[help_cnt] = sav_opts[help_cnt];
		          options[0] = "   HELP for",
                          options[12] = "Exit from Help menu";
                               help_ret =  popup_menu(
                                       background_color,
                                       text_color,
                                       div_color,
                                       menu_top,
                                       menu_left,
                                       MEN_SIZE,
				       "l_help",
				       1
                                       ) ;
	                    switch (help_ret) {
	                          case 2:
	                          case 3:
	                          case 4:
	                          case 5:
	                          case 6:
	                          case 7:
                                       Help_item(&M_label,help_ret);
	                               break;
	                          case 9:
	                          case 10:
	                          case 11:
                                       Help_item(&M_main,help_ret);
	                               break;
	                          case 12:
	                               help_for = 0;
		                       options[0] = "  Label Menu\0";
                                       background_color = D_translate_color(BC_MAIN) ;
                                       text_color       = D_translate_color(TC_MAIN) ;
                                       div_color        = D_translate_color(DC_MAIN) ;
				       erase_popup("l_help");
                                       erase_popup("info_help");
	                               break;
	                          default:
	                               break;
		                    }
                          }
                       }
	               break;
	            case 12:  /* Quit */
		            label = 0;
		            break;

	            default:
		        break;	
	            }
    }
    return (Pass);
}


ask_cat ()
{
    char buf[100];

    sprintf (buf, "Enter Category Number (0 to END) :[0]       ") ;
    message[0] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[0],"%s", buf);
    message[1] = " ";
    message[2] = " ";
    message[3] = '\0';

    Dchoose(MEN.name) ;
    popup_messg( "info", 1) ;
    popup_ques(20,&buf[0]);
    erase_popup("info");
    return (atoi (buf));
}


/* ask user to select area to label and create  new area and label */
/* returns 0 OK  or -1 no area created */
label_area  (map, cat)
    struct Map_info *map;
    int cat;
{
    int ier;
    int line, area, att;
    int ret;
    double x, y;
    struct Categories cats;

    while (1)
    {
	
 	      /* find_line_with_mouse  fills Gpoints */
	new_point_with_mouse (&x, &y, "Select point within area:");

	if (x == 0.0 && y == 0.0)
	    return (-1);

	/* change color so they know something happend */
	R_standard_color (dcolors[CLR_AMARK]); 
	Blot (&x, &y);
	local_x = x; local_y = y;	/* store these for tell_area_label() */

	local_prev = 0;	/* reset static flag */
	/* find_line loads global struct: Garea */
	if (0>=(line = find_line_with_mouse (AREA, "Select a Boundary line:", tell_area_label)))
	       {
	       unset_dot (x, y);
	       continue;
	       }

	if (make_area_label (map, line) >= 0)	/* completed an area? */
	{
          if ( ! mouse_yes_no ("Accept this area ? ")) ret = 0;
          else ret = 1;          
	  erase_popup("info");
	  Dchoose(DIG.name);

	  if (ret)
	  {
	    /* if this far, then an area is selected, either old or new */
	    /*  if local_area, then is old, else  Garea holds area info */
	    if (local_area)
	    {
		P_ATT *AP;
		char buf[100];

		if (!map->Area[local_area].att)
		{
/*DEBUG*/ debugf ("Label area: creating new attribute\n");
		    map->Area[local_area].att = 
			dig_new_att (map, local_x, local_y, AREA, local_area, cat);
		}
		else
		{
/*DEBUG*/ debugf ("Label area: attribute exists changing it\n");
		    AP = &(map->Att[map->Area[local_area].att]);
                    if (Disp_names)
                        { /* read category file , if it exists*/
                        G_suppress_warnings (1);
                        ier = G__read_cats ("SUBJ", N_name, G_mapset(), &cats, 1);
                        G_suppress_warnings (0);
                        }
                    if (Disp_names && !ier)
	               {      /* check for label available */
	               if (AP->cat <= cats.num)
		          {
	                  sprintf (buf, "%s", cats.list[AP->cat].label);
	                  G_free_cats(&cats);
		          }
                       else
		          {
                          sprintf (buf, "Category/Label error.");
                          message[0] = (char *) malloc (strlen (buf) + 1);
                          sprintf(message[0],"%s", buf);
                          sprintf (buf, " There is NO label for cat %d",
	                                AP->cat);
                          message[1] = (char *) malloc (strlen (buf) + 1);
                          sprintf(message[1],"%s", buf);
                          message[2] = " ";
                          message[3] = '\0';

                          Dchoose(MEN.name) ;
                          popup_messg( "warning", 1) ;
		          sleep(3);
                          erase_popup("warning");
                          Dchoose(DIG.name) ;
	                  sprintf (buf, "%d", AP->cat);
		          }
	               }
                   else
	             sprintf (buf, "%d", AP->cat);


		    R_standard_color (dcolors[CLR_ERASE]);
		    _Blot (&(AP->x), &(AP->y));

		    Adot (&(AP->x), &(AP->y), buf);
		    AP->cat = cat;
		    AP->x = x;
		    AP->y = y;
		    dig_update_att (map, map->Area[local_area].att);
		}

		display_area (local_area, map);
		area = local_area;
	    }
	    else
	    {
/*DEBUG*/ debugf ("Label area: new area:  ");
		area = dig_new_area (map, &Garea, 0);	/* give dummy att info*/
		if (area < 0) return (-1);		/* out of memory? */
/*DEBUG*/ debugf (" creating new attribute\n");
		att = dig_new_att (map, x, y, AREA, area, cat);	/* create new att  */
		if (att < 0)
		    return (-1);
		if (att < 0) return (-1);		/* out of memory? */
		map->Area[area].att = att;		/* stick in att info */
		display_area (area, map);
	    }
	    display_area_label (area, map, CLR_ALABEL);
	    Changes_Made = 1;
	  }
	  else	/* cleanup and leave */
	  {
	    display_line (map->Line[line].type, &Gpoints, line, map);
	    R_standard_color (dcolors[CLR_ERASE]);
	    Blot (&local_x, &local_y);
	    if (Disp_outline && local_area && AREA_LABELED (&(map->Area[local_area])))
		display_area (local_area, map);
	    else
		if (local_area)
		    reset_area (local_area, map);
		else
		    _reset_area (&Garea, map);
	  }
	}
	else  /* area not made */
	{
	    erase_popup("info");
	    R_standard_color (dcolors[CLR_ERASE]);
	    Blot (&local_x, &local_y);
	}
    }
}

label_lines  (map, cat)
    struct Map_info *map;
    int cat;
{
    int line;

    while (1)
    {
	G_clear_screen ();
	/* find_line_with_mouse  fills Gpoints */
	if (0 >= (line = find_line_with_mouse (LINE | AREA, "Choose line:", tell_line_label)))
	    {
	    return (-1);
	    }

	if (0 > label_line (map, line, cat, &Gpoints))
	    return (-1);
    }
}

label_sites  (map, cat)
    struct Map_info *map;
    int cat;
{
    int line;

    while (1)
    {
	G_clear_screen ();
	/* find_line_with_mouse  fills Gpoints */
	if (0 >= (line = find_line_with_mouse (DOT, "Choose site:", tell_line_label)))
	    {
	    return (-1);
	    }

        if (0 > label_line (map, line, cat, &Gpoints))
	    return (-1);
    }
}
    
label_line (map, line, cat, Points)
    struct Map_info *map;
    int line;
    int cat;
    struct line_pnts *Points;
{
    int att;
    double x, y;
    int line_type;
    P_ATT *Att;


    Dchoose(DIG.name);
    line = abs (line); /* dpg 11/89 */

    line_type = map->Line[line].type;

    /* area and line lines all get labelled as LINE */
    if (line_type == AREA)
	line_type = LINE;

    /* remove old label from screen */
    erase_line (map->Line[line].type, Points, line, map);

    get_line_center (&x, &y, Points);

    if (map->Line[line].att) /* if already exists, change it */
    {
	att = map->Line[line].att;
	Att = &(map->Att[att]);
	Att->cat = cat;
	Att->x = x;
	Att->y = y;
	Changes_Made = 1;
	dig_update_att (map, att);
    }
    else
    {
	att = dig_new_att (map, x, y, line_type, line, cat);
	if (att < 0)
	    return (-1);
	map->Line[line].att = att;
	Changes_Made = 1;
    }
    display_line (map->Line[line].type, Points, line, map);
    return (0);
}

unlabel_area  (map)
    struct Map_info *map;
{
    int line, area, att;
    int ier;
    double x, y;
    struct Categories cats;

    while (1)
    {
                /* find_line_with_mouse  fills Gpoints */
	new_point_with_mouse (&x, &y, "Select point within area:");

	if (x == 0.0 && y == 0.0)
	{
	    unset_dot (x, y);
	    return (-1);
	}
	R_standard_color (dcolors[CLR_AMARK]);
	Blot (&x, &y);
	local_x = x; local_y = y;	/* store these for tell_area_label() */

	if (0>=(line = find_line_with_mouse (AREA, "Select a Boundary line:", tell_area_unlabel)))
	          {
/* is a bug here.  if accept a line that does not make an area */
/* this line will stay highlit */
/* if they aborted w/out choosing a line, there will be no line */
/* lit.  the previous is the one that needs to be taken care of */
/* should probly be fixed in find_w_mouse */
	          unset_dot (x, y);
	          continue;
	          }
        erase_popup("t_area_unl");
        Dchoose(DIG.name) ;
	unset_dot (x, y);

	if (local_area && AREA_LABELED (&(map->Area[local_area])))
	{
	    P_AREA *Area;
	    P_ATT *Att;
	    char buf[50];

	    Area = &(map->Area[local_area]);
	    Changes_Made = 1;

	    Att = &(map->Att[Area->att]);	

	    if (Disp_labels)           /* remove label on screen */
	      display_area_label (local_area, map, CLR_ERASE);

	    _reset_area (Area, map);

	    dig_del_att (map, Area->att);	/* delete its attribute */
	    Area->att = 0;
	    /*  this is too drastic, lets leave the area alone
	    Del_area (map, local_area);
	    */
	}
	else
	{
/*DEBUG*/ debugf ("No attribute.  no action taken\n");
	}
    }
}

unlabel_lines  (map)
    struct Map_info *map;
{
    int line;

    while (1)
    {
	G_clear_screen ();
	/* find_line_with_mouse  fills Gpoints */
	if (0 >= (line = find_line_with_mouse (LINE | AREA, "Choose labeled line:", tell_line_label)))
	    {
	    return (-1);
	    }

	Dchoose(DIG.name);
	if (map->Line[line].att)
	{
	    erase_line (map->Line[line].type, &Gpoints, line, map);
	    dig_del_att (map, map->Line[line].att);
	    map->Line[line].att = 0;
	    Changes_Made = 1;
	    display_line (map->Line[line].type, &Gpoints, line, map);
	}
    }
}

unlabel_sites (map)
    struct Map_info *map;
{
    int line;

    while (1)
    {
	G_clear_screen ();
	/* find_line_with_mouse  fills Gpoints */
	if (0 >= (line = find_line_with_mouse (DOT, "Choose labeled Site:", tell_line_label)))
	    {
	    return (-1);
	    }

	Dchoose(DIG.name);
	if (map->Line[line].att)
	{
/*-->  added by SCS, RL Glenn, 9/20/91
	 want to remove text on sites too, same as unlabelling lines */
	    erase_line (map->Line[line].type, &Gpoints, line, map);

	    dig_del_att (map, map->Line[line].att);
	    map->Line[line].att = 0;
	    Changes_Made = 1;
	    display_line (map->Line[line].type, &Gpoints, line, map);
	}
    }
}

tell_line_label (map, line)
    struct Map_info *map;
    int line;
{
    char buf[200];

    if (map->Line[line].att)
    {
        if (map->Line[line].type == DOT)                                     
             sprintf (buf, "Site is Category %d   ", 
				 map->Att[map->Line[line].att].cat);
	else
             sprintf (buf, "Line is Category %d   ", 
				 map->Att[map->Line[line].att].cat);
        message[0] = (char *) malloc (strlen (buf) + 1);
        sprintf(message[0],"%s", buf);
    }
    else
    {    
        if (map->Line[line].type == DOT)                                    
              sprintf(buf, "Site is Not labeled   ");
        else
	      sprintf(buf, "Line is Not labeled   ");

        message[0] = (char *) malloc (strlen (buf) + 1);
        sprintf(message[0],"%s", buf);
    }
    message[1] = '\0';
    Dchoose(MEN.name) ;
    popup_messg( "info", 1) ;

    return (0);
}


/* Document this !!   return values?? */
/* called by find_line_w_mouse */
make_area_label (map, line)
    struct Map_info *map;
    int line;
{
    int area;
    char buf[200];

    if (local_prev)
	if (local_area)
	    display_area (local_area, map);
	else
	    _display_area (&Garea, map);
	
    if ((area = check_area (map, line, local_x, local_y)) > 0)
    {
	local_prev = 1;
	local_area = area;

	if (Auto_Window && area_outside_window (&(map->Area[local_area])))
	{
	    P_AREA *Area;

	    Area = &(map->Area[local_area]);
	    expand_window (Area->N, Area->S, Area->E, Area->W);
	}

	highlight_area (area, map);
	if (map->Area[area].att)
	    sprintf (buf, "Area is labeled as category %d   ", 
		map->Att[map->Area[area].att].cat);
	else
	    sprintf (buf, "Area is NOT yet labeled   ");

        message[0] = (char *) malloc (strlen (buf) + 1);
        sprintf(message[0],"%s", buf);
        message[1] = '\0';

        Dchoose(MEN.name) ;
        popup_messg( "info", 1) ;
	return (1);
    }
    else
    {
	local_area = 0;
	if (0 >= build_area (map, local_x, local_y, line, &Garea))	/* create new area */
	{
	    BEEP;
            sprintf (buf, "Could not create area   ");
            message[0] = (char *) malloc (strlen (buf) + 1);
            sprintf(message[0],"%s", buf);
            message[1] = '\0';

            Dchoose(MEN.name) ;
            popup_messg( "info", 1) ;
	    local_prev = 0;
            Dchoose(DIG.name) ;
	    display_line (AREA, &Gpoints, line, map);	/* undo highlight */
            Dchoose(MEN.name) ;
	    return (-1);	/* NO Current Area */
	}
	else
	{
	    if (Auto_Window && area_outside_window (&Garea))
		expand_window (Garea.N, Garea.S, Garea.E, Garea.W);
	    _highlight_area (&Garea, map);
	}
    }

    Dchoose(MEN.name) ;
    return (0);
}

tell_area_unlabel (map, line)
    struct Map_info *map;
    int line;
{
    int area;
    char buf[1024];

    local_area = 0;
    if ((area = check_area (map, line, local_x, local_y)) > 0 &&
	map->Area[area].att)
    {
	sprintf (buf, "Area is labeled as category %d   ", 
	    map->Att[map->Area[area].att].cat);

        message[0] = (char *) malloc (strlen (buf) + 1);
        sprintf(message[0],"%s", buf);
        message[1] = '\0';

        Dchoose(MEN.name) ;
        popup_messg( "t_area_unl", 1) ;
	local_area = area;
	return (0);
    }
    else
    {
        sprintf (buf, "Area is not labeled   ");
        message[0] = (char *) malloc (strlen (buf) + 1);
        sprintf(message[0],"%s", buf);
        message[1] = '\0';

        Dchoose(MEN.name) ;
        popup_messg( "t_area_unl", 1) ;
	local_area = 0;
	return (-1);
    }
}


tell_area_label (map, line)
    struct Map_info *map;
    int line;
{
    int area;
    char buf[1024];

    local_area = 0;
    if ((area = check_area (map, line, local_x, local_y)) > 0 &&
	map->Area[area].att)
    {
	sprintf (buf, "Area is labeled as category %d   ", 
	    map->Att[map->Area[area].att].cat);

        message[0] = (char *) malloc (strlen (buf) + 1);
        sprintf(message[0],"%s", buf);
        message[1] = '\0';
	local_area = area;
    }
    else
    {
        sprintf (buf, "Area is not labeled   ");
        message[0] = (char *) malloc (strlen (buf) + 1);
        sprintf(message[0],"%s", buf);
        message[1] = '\0';
	local_area = 0;
    }
    Dchoose(MEN.name) ;
    popup_messg( "info", 1) ;
    return (0);
}

/* given x, y  and line number,  check if x, y is within a predefined 
**  area bounded by  line
*/
check_area (map, line, x, y)
    struct Map_info *map;
    int line;
    double x, y;
{
    line = ABS (line);
/*DEBUG*/ debugf ("Check_area: line %d R %d L %d (%lf, %lf)\n", line, map->Line[line].right, map->Line[line].left, x, y);
    if (map->Line[line].right > 0)   /* ISLE */
	if (dig_point_in_area (map, x, y, &(map->Area[map->Line[line].right])) > 0.)
	{
/*DEBUG*/ debugf ("Check_area:  POINT IN AREA(right)  returned TRUE\n");
	    return (map->Line[line].right);
	}
    if (map->Line[line].left > 0)   /* ISLE */
	if (dig_point_in_area (map, x, y, &(map->Area[map->Line[line].left])) > 0.)
	{
/*DEBUG*/ debugf ("Check_area:  POINT IN AREA(left) returned TRUE\n");
	    return (map->Line[line].left);
	}
/*DEBUG*/ debugf ("Check_area:  POINT IN AREA returned FALSE\n");
    return (0);
}

/* need a point to place label on line.
**  for a line w/ > 2 points just pick the middle point
**  for a line w/ only 2 points we dont want to label the node (> 1 line)
**  so we calculate a mid-point
*/
#ifdef OLD_LINE_CENTER
get_line_center (x, y, Points)
    double *x, *y;
    struct line_pnts *Points;
{
    int which_coor;

    if (Points->n_points < 1)
	return (-1);

    if (Points->n_points < 2)
    {
	*x = Points->x[0];
	*y = Points->y[0];
	return (0);
    }
    if (Points->n_points > 2)
    {
	which_coor = Points->n_points >> 1;
	*x = Points->x[which_coor];
	*y = Points->y[which_coor];
	return (0);
    }

    /* calculate the middle of a two points line */
    *x = (Points->x[0] + Points->x[1]) / 2.0;
    *y = (Points->y[0] + Points->y[1]) / 2.0;
    return (0);
}
#else  /* new improved line_center */

/* 
** find a fast approximate point on a chain to place a label
**  uses a city block distance approximation to choose the point
**  In other words use distance x+y to approximate len of hypot
*/

/*
**  return found point in *x and *y
** return 0 on success ,-1 on error
*/

double fabs ();

get_line_center (x, y, Points)
    double *x, *y;
    struct line_pnts *Points;
{
    register int i;
    register int n_points;
    register double *ux, *uy;
    double dist;		/* running total of line length */
    double half_dist;		/* half total line length */
    double len;			/* tmp length of current line seg */
    double frac;		/* overshoot / line length */
    char buf[80];

    n_points = Points->n_points;
    ux = Points->x;
    uy = Points->y;

    if (n_points <= 0)
	return -1;
    if (n_points == 1)
    {
	*x = Points->x[0];
	*y = Points->y[0];
	return (0);
    }
	
    dist = 0.0;
    /* get total dist */
    for (i = 1 ; i < n_points ; i++)
	dist += (fabs(ux[i]-ux[i-1]) + fabs(uy[i]-uy[i-1]));
    if (dist == 0.0)
    {
	*x = Points->x[0];
	*y = Points->y[0];
	return (0);
    }

    half_dist = dist / 2.0;

    dist = 0.0;
    for (i = 1 ; i < n_points ; i++)
    {
	len = (fabs(ux[i]-ux[i-1]) + fabs(uy[i]-uy[i-1]));
	dist += len;
	if (dist >= half_dist)  /* we're there */
	{
	    frac = 1 - (dist - half_dist) / len;
	    *x = frac * (ux[i]-ux[i-1]) + ux[i-1];
	    *y = frac * (uy[i]-uy[i-1]) + uy[i-1];
	    return (0);
	}
    }

    sprintf (buf, "Get_line_center failed   ");
    message[0] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[0],"%s", buf);
    message[1] = '\0';

    Dchoose(MEN.name) ;
    popup_messg( "info", 1) ;
    sleep(2);
    erase_popup("info");
    *x = Points->x[0];
    *y = Points->y[0];
    return (-1);
}
#endif

display_labeled_areas (map)
    struct Map_info *map;
{
    display_all_areas (map);
}

display_all_areas (map)
    struct Map_info *map;
{
    register int i;
    int ret = 0 ;
    char buf[100];

    set_keyboard ();
    for (i = 1 ; i <= map->n_areas ; i++)
    {
	if (key_hit (buf))
	{
	    if (*buf == ESC)
	    {
		ret = -1;
		break;
	    }
	}
	if (AREA_LABELED (&(map->Area[i])))
	    display_area (i, map);
    }
    unset_keyboard ();
    R_flush ();
    return (ret);
}

display_labeled_lines (map)
    struct Map_info *map;
{
    register int i;
    for (i = 1 ; i <= map->n_lines ; i++)
	if (LINE_ALIVE (&(map->Line[i])) && map->Line[i].att && 
		line_in_window (&(map->Line[i]))) 
	{
	    V1_read_line (map, &Gpoints, map->Line[i].offset);
	    _display_line (map->Line[i].type, &Gpoints, i, map);
	}
    R_flush ();
}

/* this is (no longer) a hidden feature for whatever use */
label_all_lines (map, cat)
    struct Map_info *map;
    int cat;
{
    int line, att;
    double x, y;
    char buf[80];

    sprintf (buf, "Processing ...");
    message[0] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[0],"%s", buf);
    message[1] = '\0';

    Dchoose(MEN.name) ;
    popup_messg( "info", 1) ;

    for (line = 1 ; line <= map->n_lines ; line++)
    {

	/* only do this for LINE lines */
	/* if already labeled, leave it alone */
	if (LINE_ALIVE (&(map->Line[line])) && map->Line[line].type == LINE && !map->Line[line].att)
	{

	    if(0 > V1_read_line(map, &Gpoints, map->Line[line].offset))
		return (-1);
	    /*
	    erase_line (map->Line[line].type, &Gpoints, line, map);
	    */
	    get_line_center (&x, &y, &Gpoints);
	    att = dig_new_att (map, x, y, LINE, line, cat);
	    if (att < 0)
		return (-1);
	    map->Line[line].att = att;
	    Changes_Made = 1;
	    display_line (map->Line[line].type, &Gpoints, line, map);
	}
    }
    erase_popup("info");
    sprintf (buf, "   DONE   ");
    message[0] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[0],"%s", buf);
    message[1] = '\0';

    Dchoose(MEN.name) ;
    popup_messg( "info", 1) ;
    sleep(2);
    erase_popup("info");
}


#ifdef SCS_MODS
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
	if (Type == 1)         /* LINE */
	    {
            sprintf(buffr,
	    " Do you wish to enter line names? ");
	    ans = mouse_yes_no (buffr);
	    }
	else if (Type == 2)    /* AREA */
	    {
            sprintf(buffr,
	    " Do you wish to enter area names? ");
	    ans = mouse_yes_no (buffr);
	    }
	else if (Type == 4)     /* DOT */
	    {
            sprintf(buffr,
	    " Do you wish to enter site names? ");
	    ans = mouse_yes_no (buffr);
	    }
	else                   /* PSU  always enter names */
	    ans = 1;

        if (ans)
	{
	    /* Make Master Category dir, if not existing */
	    G__make_mapset_element("SUBJ") ;
	    while(1)
	    {
		ans = -1;
		while(ans == -1)
		{
                    sprintf (buffr," Enter the SUBJECT matter ");
                    message[0] = (char *) malloc (strlen (buffr) + 1);
                    sprintf(message[0],"%s", buffr);
                    sprintf (buffr," Enter 'list' for available Subject files    ");
                    message[1] = (char *) malloc (strlen (buffr) + 1);
                    sprintf(message[1],"%s", buffr);
                    sprintf (buffr," <CR> to Abort/Quit): ");
                    message[2] = (char *) malloc (strlen (buffr) + 1);
                    sprintf(message[2],"%s", buffr);
                    message[3] = " ";
                    message[4] = " ";
                    message[5] = '\0';

                    Dchoose(MEN.name) ;
                    popup_messg( "info1", 1) ;
                    popup_ques(20,&buffr[0]);
		    if (strlen(buffr) == 0) 
		        {
			erase_popup("info1");
			return(0);
			}

		    else if (strcmp(buffr,"list") == 0) 
		        {
		        erase_popup("info1");
                        sprintf (buffr, "See terminal for information   ");
                        message[0] = (char *) malloc (strlen (buffr) + 1);
                        sprintf(message[0],"%s", buffr);
                        message[1] = '\0';

                        Dchoose(MEN.name) ;
                        popup_messg( "info2", 1) ;
			G_clear_screen ();
			list("SUBJ");
		        erase_popup("info2");
			G_clear_screen ();
			}
		    else
			ans = 1;
		}
              
		erase_popup("info1");
		if (ans == 0) return(0);
		N_subj_file = G_store(buffr);

		/* read category file , if it exists*/
		G_suppress_warnings (1);
		ier = G__read_cats ("SUBJ", N_subj_file, G_mapset(), pcats, 1);
		G_suppress_warnings (0); 
		if (ier < 0)
		{ 
		    if (Type == PSU)
		    {
                        sprintf (buffr,
			     "PSU SUBJ file must already exist   ");
                        message[0] = (char *) malloc (strlen (buffr) + 1);
                        sprintf(message[0],"%s", buffr);
                        message[1] = '\0';

                        Dchoose(MEN.name) ;
                        popup_messg( "warning", 1) ;
			sleep (3);
			erase_popup("warning");
			return -1;
		    }
		    sprintf(buffr," Do you want to create   \nSUBJ/ file <%s>? ",N_subj_file);
                    ans2 = mouse_yes_no (buffr);
		    if (ans2)
		    {
			G_init_cats ((CELL)0, N_subj_file, pcats);
			G_set_cat ((CELL)0, "no data", pcats); 
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
	if (ans == 0)
	     {
	     Disp_names = 0;
	     return(0);
	     }
    }  /* end while */
    return (0);
}

ask_name(pcats)
    struct Categories *pcats ;
{
    int i, icode, recd, ier;
    char buffr[128], area_name[40], cat_name[40];
    char *nptr, *cptr ;

    while (1)
      {
      sprintf (buffr,"   Enter a label (<ESC> to END): ");
      message[0] = (char *) malloc (strlen (buffr) + 1);
      sprintf(message[0],"%s", buffr);
      message[1] = " ";
      message[2] = " ";
      message[3] = '\0';

      Dchoose(MEN.name) ;
      popup_messg( "info1", 1) ;
      popup_ques(20,&buffr[0]);
      if (*buffr == '\100')
	 {
	 erase_popup("info1");
         return(0);
         }

      erase_popup("info1");
      strcpy(area_name,buffr);
      nptr = area_name;

	/* find input string in category struct, assign category value to the
		    area_name based on category file record number*/
      recd = pcats->count;             /* set the number of categories */
      for (i=0;i<recd;i++)                /* category search */
	{		 
	                                    /* get a category label */
        sscanf (pcats->list[i].label, "%s", cat_name);
	cptr = cat_name;                /* first part only */

	if (strcmp(nptr,cptr) == 0)     /* compare for match */
	   {                           /* match, assigned already */
	   icode = pcats->list[i].num; /* set icode to category code */
	   return(icode);
	   }
	} 
	/* end of category search, NO category names match */

      sprintf(buffr," Add new category <%d>\nnamed <%s> ? ",recd,nptr);
      if (mouse_yes_no (buffr))
	 {                                      /* user said YES */
	 G_set_cat ((CELL)recd, nptr, pcats);  /* create entry */
         ier = G__write_cats ("SUBJ", N_subj_file, pcats);
         if (ier < 0)
            { 
            sprintf(buffr," Error in writting SUBJ file");
            message[0] = (char *) malloc (strlen (buffr) + 1);
            sprintf(message[0],"%s", buffr);
            sprintf(buffr," <%s>", N_subj_file);
            message[1] = (char *) malloc (strlen (buffr) + 1);
            sprintf(message[1],"%s", buffr);
            message[2] = " ";
            message[3] = '\0';

            Dchoose(MEN.name) ;
            popup_messg( "warning", 1) ;
            sleep(2);
            erase_popup("warning");
	    return(0);
            }
	 else
	    {
	    subj_changed = 1;
	    Disp_names = 0;
	    }
	 return(recd);
	 }
      }   /* end of while */
}

get_cat(pcats)
    struct Categories *pcats ;
{
    int i, icode, recd, ier;
    char buffr[128], area_name[40], cat_name[40];
    char *nptr, *cptr ;

    while (1)
      {
	/* find category string in category struct, which is NULL */

      recd = pcats->count;             /* set the number of categories */
      for (i=0;i<recd;i++)                /* category search */
	{		 
	                                    /* get a category label */
/*fprintf(stderr,"recd[%d]: %d  <%s>, len= %d\n",i,pcats->list[i].num,pcats->list[i].label,strlen(pcats->list[i].label));
sleep(2);*/
	if (strlen(pcats->list[i].label) == 0)     /* got one */
	   {                           
	   icode = pcats->list[i].num; /* set icode to category code */
           sprintf (buffr, "   Category <%d> available:  ",icode);
           message[0] = (char *) malloc (strlen (buffr) + 1);
           sprintf(message[0],"%s", buffr);
           sprintf (buffr, "   Enter a label: ");
           message[1] = (char *) malloc (strlen (buffr) + 1);
           sprintf(message[1],"%s", buffr);
           message[2] = " ";
           message[3] = " ";
           message[4] = '\0';

           Dchoose(MEN.name) ;
           popup_messg( "info1", 1) ;
           popup_ques(20,&buffr[0]);
           erase_popup("info1");
/*         if (!strlen(buffr)) 
	      {
              return(0);
              }*/

           strcpy(area_name,buffr);
           nptr = area_name;
	   G_set_cat ((CELL)i, nptr, pcats);  /* put in label entry */
           ier = G__write_cats ("dig_cats", N_name, pcats);
           if (ier < 0)
              { 
              sprintf(buffr," Error in writting dig_cats file");
              message[0] = (char *) malloc (strlen (buffr) + 1);
              sprintf(message[0],"%s", buffr);
	      sprintf(buffr," <%s>", N_cats_file);
              message[1] = (char *) malloc (strlen (buffr) + 1);
              sprintf(message[1],"%s", buffr);
              message[2] = " ";
              message[3] = '\0';

              Dchoose(MEN.name) ;
              popup_messg( "warning", 1) ;
	      sleep(2);
              erase_popup("warning");
	      break;
              }
           else cats_changed = 1;
	   return(icode);
	   }
	} 
	/* end of category search, NO category names are NULL */

      sprintf(buffr," Add new category <%d> ? ",recd);
      if (mouse_yes_no (buffr))
	 {                                      /* user said YES */
         sprintf (buffr, "   Enter a label: ");
         message[0] = (char *) malloc (strlen (buffr) + 1);
         sprintf(message[0],"%s", buffr);
         message[1] = " ";
         message[2] = " ";
         message[3] = '\0';

         Dchoose(MEN.name) ;
         popup_messg( "info1", 1) ;
         popup_ques(20,&buffr[0]);
         erase_popup("info1");
         if (!strlen(buffr)) 
	      {
              return(0);
              }

         strcpy(area_name,buffr);
         nptr = area_name;
	 G_set_cat ((CELL)recd, nptr, pcats);  /* create entry */
         ier = G__write_cats ("dig_cats", N_name, pcats);
         if (ier < 0)
            { 
            sprintf(buffr," Error in writting dig_cats file ");
            message[0] = (char *) malloc (strlen (buffr) + 1);
            sprintf(message[0],"%s", buffr);
	    sprintf(buffr," <%s>", N_cats_file);
            message[1] = (char *) malloc (strlen (buffr) + 1);
            sprintf(message[1],"%s", buffr);
            message[2] = " ";
            message[3] = '\0';

            Dchoose(MEN.name) ;
            popup_messg( "warning", 1) ;
	    sleep(2);
            erase_popup("warning");
	    break;
            }
         else cats_changed = 1;
	 return(recd);
	 }
      else 
	 {
	 return(0);
	 }
      }   /* end of while */
}

label_psu  (map, cat)
    struct Map_info *map;
    int cat;
{
    int ier;
    int line, area, att;
    double x, y;
    int ret;
    struct Categories cats;

	G_clear_screen ();
	         /* find_line_with_mouse  fills Gpoints */
	new_point_with_mouse (&x, &y, "Select point within area:");

	if (x == 0.0 && y == 0.0)
	    return (-1);

	/* change color so they know something happend */
	R_standard_color (dcolors[CLR_AMARK]); 
	Blot (&x, &y);
	local_x = x; local_y = y;	/* store these for tell_area_label() */

	local_prev = 0;	/* reset static flag */
	/* find_line loads global struct: Garea */
	if (0>=(line = find_line_with_mouse (AREA, "Select a Boundary line:", tell_area_label)))
	       {
	       unset_dot (x, y);
	       return (-1);
	       }

	if (make_area_label (map, line) >= 0)	/* completed an area? */
	{
	  if ( !  mouse_yes_no ("Accept this area ? ")) ret = 0;
          else ret = 1;
	  erase_popup("info");

	  Dchoose(DIG.name);
          if (ret)
	  {
	    /* if this far, then an area is selected, either old or new */
	    /*  if local_area, then is old, else  Garea holds area info */
	    if (local_area)
	    {
		P_ATT *AP;
		char buf[100];

		if (!map->Area[local_area].att)
		{
/*DEBUG*/ debugf ("Label area: creating new attribute\n");
		    map->Area[local_area].att = 
			dig_new_att (map, local_x, local_y, AREA, local_area, cat);
		}
		else
		{
/*DEBUG*/ debugf ("Label area: attribute exists changing it\n");
		    AP = &(map->Att[map->Area[local_area].att]);
                    if (Disp_names)
                        { /* read category file , if it exists*/
                        G_suppress_warnings (1);
                        ier = G__read_cats ("SUBJ", N_name, G_mapset(), &cats, 1);
                        G_suppress_warnings (0);
                        }
                    if (Disp_names && !ier)
	               {      /* check for label available */
	               if (AP->cat <= cats.num)
		          {
	                  sprintf (buf, "%s", cats.list[AP->cat].label);
	                  G_free_cats(&cats);
		          }
                       else
		          {
                          sprintf (buf, "Category/Label error");
                          message[0] = (char *) malloc (strlen (buf) + 1);
                          sprintf(message[0],"%s", buf);
                          sprintf (buf, " There is NO label for cat %d   ",
	                                AP->cat);
                          message[1] = (char *) malloc (strlen (buf) + 1);
                          sprintf(message[1],"%s", buf);
                          message[2] = " ";
                          message[3] = '\0';

                          Dchoose(MEN.name) ;
                          popup_messg( "warning", 1) ;
		          sleep(3);
                          erase_popup("warning");
                          Dchoose(DIG.name) ;
	                  sprintf (buf, "%d", AP->cat);
		          }
	               }
                   else
	             sprintf (buf, "%d", AP->cat);

		    R_standard_color (dcolors[CLR_ERASE]);
		    _Blot (&(AP->x), &(AP->y));
		    Adot (&(AP->x), &(AP->y), buf);
		    AP->cat = cat;
		    AP->x = x;
		    AP->y = y;
		    dig_update_att (map, map->Area[local_area].att);
		}

		display_area (local_area, map);
		area = local_area;
	    }
	    else
	    {
/*DEBUG*/ debugf ("Label area: new area:  ");
		area = dig_new_area (map, &Garea, 0);	/* give dummy att info*/
		if (area < 0) return (-1);		/* out of memory? */
/*DEBUG*/ debugf (" creating new attribute\n");
		att = dig_new_att (map, x, y, AREA, area, cat);	/* create new att  */
		if (att < 0)
		    return (-1);
		if (att < 0) return (-1);		/* out of memory? */
		map->Area[area].att = att;		/* stick in att info */
		display_area (area, map);
	    }
	    display_area_label (area, map, CLR_ALABEL);
	    Changes_Made = 1;
	  }
	  else	/* cleanup and leave */
	  {
	    display_line (map->Line[line].type, &Gpoints, line, map);
	    R_standard_color (dcolors[CLR_ERASE]);
	    Blot (&local_x, &local_y);
	    if (Disp_outline && local_area && AREA_LABELED (&(map->Area[local_area])))
		display_area (local_area, map);
	    else
		if (local_area)
		    reset_area (local_area, map);
		else
		    _reset_area (&Garea, map);
	  }
	}
	else  /* area not made */
	{
	    erase_popup("info");
	    R_standard_color (dcolors[CLR_ERASE]);
	    Blot (&local_x, &local_y);
	}
}

T__read_cats (element, name, mapset, pcats, full)
    char *element;
    char *name ;
    char *mapset ;
    struct Categories *pcats ;
{
    FILE *fd ;
    char buff[258], *calloc() ;
    CELL cat;
    int i, old;
    long num;


    if (!(fd = G_fopen_old (element, name, mapset)))
	return -2 ;

/* Read the number of categories */
    if (G_getl(buff,sizeof buff,fd) == NULL)
	goto error;

    if (sscanf ( buff, "# %ld"   , &num) == 1)
	old = 0;
    else if (sscanf ( buff, "%ld"   , &num) == 1)
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
    if (G_getl(buff,sizeof buff,fd) == NULL)
	goto error;
    G_strip (buff);
    G_ascii_check(buff) ;
    G_init_cats ((CELL)num, buff, pcats);

#ifdef SYSV
        {
	    long len;
	    long nalloc;

	   /* pre-allocate memory for possiblily LARGE cats file */

            pcats->count = num + 1;
	    nalloc = num + 1;
	    len = (long) nalloc * sizeof(struct Cat_List) ;
	    if ((len % 16) != 0) len = len + (len % 16);

	    if (len != (int) len) /* make sure len doesn't overflow int */
		       return -1;
    	    pcats->list = (struct Cat_List *) G_calloc(nalloc, sizeof(struct Cat_List));
	    pcats->nalloc = nalloc;
	}
#endif

    if (!old)
    {
	char fmt[256];
	float m1,a1,m2,a2;
	if (G_getl(fmt,sizeof fmt,fd) == NULL)
		goto error;
/* next line contains equation coefficients */
	if (G_getl(buff,sizeof buff,fd) == NULL)
		goto error;
	if(sscanf(buff, "%f %f %f %f", &m1, &a1, &m2, &a2) != 4)
		goto error;
	G_set_cats_fmt (fmt, m1, a1, m2, a2, pcats);
    }

/* Read all category names */
    for (cat=0;;cat++) 
    {
	char label[256];
	if (G_getl(buff, sizeof buff, fd) == 0)
	    break;
	if (old)
	    G_set_cat (cat, buff, pcats) ;
	else
	{
	    *label = 0;
	    if (sscanf (buff, "%ld:%[^\n]", &num, label) < 1)
		goto error;

#ifdef SYSV
            G_ascii_check (label);
            G_strip (label) ;      
            pcats->list[num].num = num;
            pcats->list[num].label = G_store(label) ;
            if (num > pcats->num)
               pcats->num = num;   
#else
  	    G_set_cat ((CELL)num, label, pcats);
#endif
	}
    }
    G_sort_cats (pcats);

    fclose (fd);
    return 0 ;
error:
    fclose (fd);
    return -1 ;
}
#endif /* SCS_MODS */
