/*  %W%  %G%  */
/****************************************************************************
 * popup_menu(back_colr, text_colr, div_colr, top, left, percent_per_line, popup_name, sav_opt)
 *    int back_colr ;           color of window
 *    int text_color ;          color of text and border
 *    int div_color ;           color of item divider lines
 *    int left, top ;           pixle coordinates of top-left corner
 *                              (Coordinate system is 0,0 lower left, 
 *                              100,100 upper right)
 *    int percent_per_line ;    percent of entire window per line of text
 *    char *popup_name;         name to save display panel
 *    int sav_opt;              0/1 not save/save this panel
 *
 * The bottom-right coordinates are calculated based on the top-left coors.,
 * the percent_per_line, the number of options, and the longest option text
 * length.
 *
 * - Current screen contents are stashed away in the area.
 * - Area is blanked with the background color and fringed with the
 *    text color.
 * - Options are drawn using the current font.
 * - User uses the mouse to choose the desired option.
 * - Area is restored with the original contents.
 * - Number of this option is returned to the calling program.
 ***************************************************************************/
#include "digit.h"
#include "gis.h"
#include "popup.h"

#define Y_BORDER	5
#define X_BORDER	5

popup_menu(back_colr, text_colr, div_colr, top, left, percent_per_line, p_name, sav_opt)
    char *p_name ;
{
    int t, l, b, r ;
    int opt ;
    int row ;
    int x, y ;
    int button ;
    int text_size ;
    int text_raise ;
    int n_options ;
    int max_len ;
    int len ;
    char *panel, menu_line[50] ;
    int dots_per_line , scr_len;
    int	men_top,    men_bot,    men_left,    men_right;
    float text_scale=.8 ;

/* Figure the number of options and the max length of options */
    max_len = 0 ;
    for(n_options=0; options[n_options] != NULL; n_options++)
    {
	len = strlen(options[n_options]) ;
	if (max_len < len) max_len = len ;
    }

/*  if (max_len > 50) max_len-=16;
    else if (max_len > 40) max_len-=12;*/
    if (max_len > 30) max_len-=6;
    else if (max_len > 25) max_len-=5;
    else if (max_len > 20) max_len-=4;
    else if (max_len > 15) max_len-=3;
    else if (max_len > 10) max_len-=2;
    else ;

    D_get_screen_window ( &men_top, &men_bot, &men_left, &men_right);
    scr_len = men_right - men_left;

/* Figure the dots per line  and intial text size */
    dots_per_line = (men_bot - men_top) * percent_per_line / 100 ;
    text_size = (int)(text_scale * (float)dots_per_line) ;
    text_raise = (dots_per_line - text_size + 1) / 2;
    if (text_raise == 0)
	text_raise = 1;

/* Figure the initial menu size */
    t = men_bot - (men_bot - men_top) * top / 100 ;
    b = Y_BORDER + t + dots_per_line * n_options ;
    l = men_left + (scr_len) * left / 100 ;
    r = l + text_size * max_len ;

	/* see if right side is outside window */
    while (r >= men_right )  /* make the menu smaller */
       {
       l = men_left + (scr_len) * MEN_LEFT / 100 ;

       text_scale-=.01;
       text_size = (int)(text_scale * (float)dots_per_line) ;
       text_raise = (dots_per_line - text_size + 1) / 2;
       if (text_raise == 0)
	   text_raise = 1;
       r = l + text_size * max_len ;
       }

	/* see if bottom side is outside window */
    while (b >= men_bot )  /* make the menu smaller */
       {
       top+=2;
       t = men_bot - (men_bot - men_top) * top / 100 ;
       b = Y_BORDER + t + dots_per_line * n_options ;
       }
    if(sav_opt)
       {
/*fprintf(stderr,"popup_menu:  sav panel <%s>, opt= %d\n",p_name,sav_opt);*/
       R_panel_save(p_name, t, b, l, r) ;
       }

/* Clear the panel */
    R_standard_color(back_colr) ;

    R_box_abs(l, t, r, b);

/* Draw border */
    R_standard_color(text_colr) ;
    R_move_abs(l+1, t+1) ;
    R_cont_abs(r-1, t+1) ;
    R_cont_abs(r-1, b-1) ;
    R_cont_abs(l+1, b-1) ;
    R_cont_abs(l+1, t+1) ;

/* Prepare for text */
    R_font ("romans");
    R_text_size(text_size, text_size) ;

/* list the options */
    for(opt=1; opt<=n_options; opt++)
    {
	if (opt != n_options)
	{
	    R_standard_color(div_colr) ;
	    R_move_abs(l+2, t + Y_BORDER + opt * dots_per_line) ;
	    R_cont_rel (r-l-4, 0);
	}
	R_standard_color(text_colr) ;
	R_move_abs(l + X_BORDER, t + Y_BORDER + opt * dots_per_line - text_raise) ;
	sprintf(menu_line,"%s\0",options[opt-1]);
	R_text(menu_line) ;
    }

    R_flush() ;

    x = (l + r) / 2 ;
    y = (t + b) / 2 ;

    while(1)
    {
	int n;

	R_get_location_with_pointer(&x, &y, &button) ;
	if (x > r
	||  x < l
	||  y < t + Y_BORDER + dots_per_line
	||  y > b - Y_BORDER)
		continue ;

	n = y - t - Y_BORDER ;
	if (n % dots_per_line == 0) continue;
        Next_t = -1 * (((y - men_bot) * 100) / (men_bot - men_top));
        Next_l = ((x - men_left) * 100) / (men_right - men_left);

	return (n / dots_per_line) ;
    }
}

popup_butns(top, left, buttns, b_name, sav_opt)
    char *buttns[], *b_name ;
{
    int t, l, b, r ;
    int opt ;
    int row , x, y ;
    int text_size , text_raise ;
    int n_buttns , max_len , len ;
    char *panel, menu_line[50] ;
    int back_colr, text_colr, div_colr;
    int percent_per_line ;
    int dots_per_line , scr_len;
    int	men_top,    men_bot,    men_left,    men_right;
    float text_scale=.8 ;

    percent_per_line = MEN_SIZE;

    back_colr 	    = D_translate_color(BC_BUTN) ;
    text_colr       = D_translate_color(TC_BUTN) ;
    div_colr        = D_translate_color(DC_BUTN) ;

/*fprintf(stderr,"popup_butns:  sav panel <%s>, opt= %d\n",b_name,sav_opt); */
/* Figure the number of buttns and the max length of buttns */
    max_len = 0 ;
    for(n_buttns=0; buttns[n_buttns] != NULL; n_buttns++)
    {
/*fprintf(stderr,"buttns[%d]: <%s>\n",n_buttns,buttns[n_buttns]);*/
	len = strlen(buttns[n_buttns]) ;
	if (max_len < len) max_len = len ;
    }

    if (max_len > 50) max_len-=16;
    else if (max_len > 40) max_len-=12;
    else if (max_len > 30) max_len-=8;
    else if (max_len > 25) max_len-=5;
    else if (max_len > 20) max_len-=4;
    else if (max_len > 15) max_len-=3;
    else if (max_len > 10) max_len-=2;
    else ;

    D_get_screen_window ( &men_top, &men_bot, &men_left, &men_right);
    scr_len = men_right - men_left;

/* Figure the dots per line  and intial text size */
    dots_per_line = (men_bot - men_top) * percent_per_line / 100 ;
    text_size = (int)(text_scale * (float)dots_per_line) ;
    text_raise = (dots_per_line - text_size + 1) / 2;
    if (text_raise == 0)
	text_raise = 1;

/* Figure the initial menu size */
    t = men_bot - (men_bot - men_top) * top / 100 ;
    b = Y_BORDER + t + dots_per_line * n_buttns ;
    l = men_left + (scr_len) * left / 100 ;
    r = l + text_size * max_len ;

	/* see if right side is outside window */
    while (r >= men_right )  /* make the menu smaller */
       {
       l = men_left + (scr_len) * MEN_LEFT / 100 ;
       text_scale-=.01;
       text_size = (int)(text_scale * (float)dots_per_line) ;
       text_raise = (dots_per_line - text_size + 1) / 2;
       if (text_raise == 0)
	   text_raise = 1;
       r = l + text_size * max_len ;
       }

	/* see if bottom side is outside window */
    while (b >= men_bot )  /* make the menu smaller */
       {
       top+=2;
       t = men_bot - (men_bot - men_top) * top / 100 ;
       b = Y_BORDER + t + dots_per_line * n_buttns ;
       }

/* Save the panel in memory */
    Dchoose(MEN.name) ;
/*fprintf(stderr,"popup_butns:  sav panel <%s>, opt= %d\n",b_name,sav_opt);*/
    if (sav_opt) 
       {
       R_panel_save(b_name, t, b, l, r) ;
       }

/* Clear the panel  */
    R_standard_color(back_colr) ;

    R_box_abs(l, t, r, b);

/* Draw border */
    R_standard_color(text_colr) ;
    R_move_abs(l+1, t+1) ;
    R_cont_abs(r-1, t+1) ;
    R_cont_abs(r-1, b-1) ;
    R_cont_abs(l+1, b-1) ;
    R_cont_abs(l+1, t+1) ;

/* Prepare for text */
    R_font ("romans");
    R_text_size(text_size, text_size) ;

/* list the buttns */
    for(opt=1; opt<=n_buttns; opt++)
    {
	if (opt != n_buttns)
	{
	    R_standard_color(div_colr) ;
	    R_move_abs(l+2, t + Y_BORDER + opt * dots_per_line) ;
	    R_cont_rel (r-l-4, 0);
	}
	R_standard_color(text_colr) ;
	R_move_abs(l + X_BORDER, t + Y_BORDER + opt * dots_per_line - text_raise) ;
/*#ifdef SYSV
	sprintf(menu_line,"@%s\0",buttns[opt-1]);
#else*/

	sprintf(menu_line,"%s\0",buttns[opt-1]);

/*#endif*/
	R_text(menu_line) ;
    }

    R_flush() ;
}

popup_messg(m_name, sav_opt)
    char *m_name ;
{
    int t, l, b, r ;
    int opt ;
    int row , x, y , top, left;
    int text_size, text_raise ;
    int n_messgs , max_len ,len ;
    int back_colr, text_colr, div_colr;
    int percent_per_line ;
    char *panel, menu_line[50] ;
    int dots_per_line , scr_len;
    int	men_top,    men_bot,    men_left,    men_right;
    float text_scale=.8 ;

    if (*m_name == '\167') 
       { /* [w]arning  */
       back_colr       = D_translate_color(BC_WARN) ;
       text_colr       = D_translate_color(TC_WARN) ;
       div_colr        = D_translate_color(DC_WARN) ;
       }
    else
       {  /* info */
       back_colr       = D_translate_color(BC_INFO) ;
       text_colr       = D_translate_color(TC_INFO) ;
       div_colr        = D_translate_color(DC_INFO) ;
       }

    top = 5;
    left = 2;
    percent_per_line = MEN_SIZE * 2;

/* Figure the number of messgs and the max length of messgs */
    max_len = 0 ;
    for(n_messgs=0; message[n_messgs] != NULL; n_messgs++)
    {
/*fprintf(stderr,"message[%d]: <%s>\n",n_messgs,message[n_messgs]);*/
	len = strlen(message[n_messgs]) ;
	if (max_len < len) max_len = len ;
    }

    if (max_len > 50) max_len-=16;
    else if (max_len > 40) max_len-=12;
    else if (max_len > 30) max_len-=8;
    else if (max_len > 25) max_len-=5;
    else if (max_len > 20) max_len-=4;
    else if (max_len > 15) max_len-=3;
    else if (max_len > 10) max_len-=2;
    else ;

    D_get_screen_window ( &men_top, &men_bot, &men_left, &men_right);
    scr_len = men_right - men_left;

/* Figure the dots per line  and intial text size */
    dots_per_line = (men_bot - men_top) * percent_per_line / 100 ;
    text_size = (int)(text_scale * (float)dots_per_line) ;
    text_raise = (dots_per_line - text_size + 1) / 2;
    if (text_raise == 0)
	text_raise = 1;

/* Figure the initial menu size */
    t = men_bot - (men_bot - men_top) * top / 100 ;
    b = Y_BORDER + t + dots_per_line * n_messgs ;
    l = men_left + (scr_len) * left / 100 ;
    r = l + text_size * max_len ;

	/* see if right side is outside window */
    while (r >= men_right )  /* make the menu smaller */
       {
       l = men_left + (scr_len) * left / 100 ;
       text_scale-=.01;
       text_size = (int)(text_scale * (float)dots_per_line) ;
       text_raise = (dots_per_line - text_size + 1) / 2;
       if (text_raise == 0)
	   text_raise = 1;
       r = l + text_size * max_len ;
       }

	/* see if bottom side is outside window */
    while (b >= men_bot )  /* make the menu smaller */
       {
       top+=2;
       t = men_bot - (men_bot - men_top) * top / 100 ;
       b = Y_BORDER + t + dots_per_line * n_messgs ;
       }

/* Save the panel in memory */
    Dchoose(MEN.name) ;
/*fprintf(stderr,"popup_mess:  sav panel <%s>, opt= %d\n",m_name,sav_opt);*/
    if (sav_opt) 
       {
       R_panel_save(m_name, t, b, l, r) ;
       }
/* Clear the panel  */
    R_standard_color(back_colr) ;

    R_box_abs(l, t, r, b);

/* Draw border */
    R_standard_color(text_colr) ;
    R_move_abs(l+1, t+1) ;
    R_cont_abs(r-1, t+1) ;
    R_cont_abs(r-1, b-1) ;
    R_cont_abs(l+1, b-1) ;
    R_cont_abs(l+1, t+1) ;
       
/* Prepare for text */
    R_font ("romans");
    R_text_size(text_size, text_size) ;

/* list the message */
    for(opt=1; opt<=n_messgs; opt++)
    {
	if (opt != n_messgs)
	{
	    R_standard_color(div_colr) ;
	    R_move_abs(l+2, t + Y_BORDER + opt * dots_per_line) ;
	    R_cont_rel (r-l-4, 0);
	}
	R_standard_color(text_colr) ;
	R_move_abs(l + X_BORDER, t + Y_BORDER + opt * dots_per_line - text_raise) ;
/*#ifdef SYSV
	sprintf(menu_line,"@%s\0",message[opt-1]);
#else*/

	sprintf(menu_line,"%s\0",message[opt-1]);

/*#endif*/
	R_text(menu_line) ;
    }

    R_flush() ;
}

erase_popup(name)
char *name;
{
/*printf(stderr,"erase panel <%s>\n",name); */
                Dchoose(MEN.name) ;
	        R_panel_restore(name) ;
	        R_panel_delete(name) ;
}

popup_ques(max_len, answer)
    char *answer ;
{
    int t, l, b, r ;
    int row ;
    int x, y, top, left ;
    int button ;
    int text_size ;
    int text_raise ;
    int len=1 ;
    int back_colr, text_colr, div_colr;
    int percent_per_line ;
    char *ptr, menu_line[50], buf[3] ;
    int dots_per_line , scr_len;
    int	men_top,    men_bot,    men_left,    men_right;
    float text_scale=.8 ;

    back_colr       = D_translate_color(BC_QUES) ;
    text_colr       = D_translate_color(TC_QUES) ;
    div_colr        = D_translate_color(DC_QUES) ;
    top = 5;
    left = 2;
    percent_per_line = MEN_SIZE * 2;
    
    Dchoose(MEN.name);
    D_get_screen_window ( &men_top, &men_bot, &men_left, &men_right);
    scr_len = men_right - men_left;

/* Figure the dots per line  and intial text size */
    dots_per_line = (men_bot - men_top) * percent_per_line / 100 ;
    text_size = (int)(text_scale * (float)dots_per_line) ;
    text_raise = (dots_per_line - text_size + 1) / 2;
    if (text_raise == 0)
	text_raise = 1;

/* Figure the initial menu size */
    t = men_bot - (men_bot - men_top) * top / 100 ;
    b = Y_BORDER + t + dots_per_line ;
    l = men_left + (scr_len) * left / 100 ;
    r = l + text_size * (max_len - 1) ;

	/* see if right side is outside window */
    while (r >= men_right )  /* make the menu smaller */
       {
       l = men_left + (scr_len) * MEN_LEFT / 100 ;

       text_scale-=.01;
       text_size = (int)(text_scale * (float)dots_per_line) ;
       text_raise = (dots_per_line - text_size + 1) / 2;
       if (text_raise == 0)
	   text_raise = 1;
       r = l + text_size * max_len ;
       }

	/* see if bottom side is outside window */
    while (b >= men_bot )  /* make the menu smaller */
       {
       top+=2;
       t = men_bot - (men_bot - men_top) * top / 100 ;
       b = Y_BORDER + t + dots_per_line ;
       }

    R_panel_save("ques", t, b, l, r) ;

/* Clear the panel */
    R_standard_color(back_colr) ;
    R_box_abs(l, t, r, b);

/* Draw border */
    R_standard_color(text_colr) ;
    R_move_abs(l+1, t+1) ;
    R_cont_abs(r-1, t+1) ;
    R_cont_abs(r-1, b-1) ;
    R_cont_abs(l+1, b-1) ;
    R_cont_abs(l+1, t+1) ;

/* Prepare for text */
    R_font ("romans");
    R_text_size(text_size, text_size) ;

/* show the quest. space */
    R_standard_color(div_colr) ;
    R_move_abs(l+2, t + Y_BORDER + dots_per_line) ;
    R_cont_rel (r-l-4, 0);
    R_standard_color(text_colr) ;

    ptr = menu_line;
/*#ifdef SYSV
**      sprintf(menu_line,"@________________________________________");**
	sprintf(menu_line,"@>>                                      ");
	ptr+=3;
#else
**	sprintf(menu_line,"________________________________________");*/

	sprintf(menu_line,">>                                      ");
	ptr+=2;

/*#endif */

    while (len <= max_len)
       {
       len++;
       ptr++;
       }
    *(ptr) = '\0';
    len = 1;

    x = (l + r) / 2 ;
    y = (t + b) / 2 ;

    ptr = menu_line;
/*#ifdef SYSV
	ptr+=3;
#else*/

	ptr+=2;

/*#endif*/

    R_move_abs(l + X_BORDER, t + Y_BORDER + dots_per_line - text_raise) ;
    R_text(menu_line) ;
    R_flush() ;

    set_keyboard ();		/* setup for key_hit () */
    while (len <= max_len)
      {
      if (key_hit (buf))
	{
	if ( (*buf > '\040' && *buf < '\077') ||   /* 0-9 & punct. char*/
	     (*buf > '\100' && *buf < '\133') ||   /* A-Z */
	     (*buf > '\140' && *buf < '\173') ||   /* a-z */
	     *buf == '\137')                       /* under score */
	     {
	     *(ptr) = *buf;
	     ptr++;
	     len++;
             }
        else if (*buf == ESC || *buf == C_C)  /* escape or ctrl-c */
	   {
	   *(ptr) = '\100';
	   break;
	   }
	else if (*buf == CR || *buf == NL) /* return or newline */
	   {
	   *(ptr) = '\0';
	   break;
	   }
        else if (*buf == BS)   /* backspace */
	   {
	   if (len == 1) continue;
	   len--;
	   ptr--;
	   *(ptr) = '\040';    /* space */
           R_standard_color(back_colr) ;
           R_box_abs(l, t, r, b);
           R_standard_color(text_colr) ;
           R_move_abs(l+1, t+1) ;
           R_cont_abs(r-1, t+1) ;
           R_cont_abs(r-1, b-1) ;
           R_cont_abs(l+1, b-1) ;
           R_cont_abs(l+1, t+1) ;
           R_standard_color(div_colr) ;
           R_move_abs(l+2, t + Y_BORDER + dots_per_line) ;
           R_cont_rel (r-l-4, 0);
           R_standard_color(text_colr) ;
           R_move_abs(l + X_BORDER,t + Y_BORDER + dots_per_line - text_raise) ;
           R_text(menu_line) ;
           R_flush() ;
	   }
        else   ;    /* ignore anything else */
        }
      else  continue;

      R_move_abs(l + X_BORDER,t + Y_BORDER + dots_per_line - text_raise) ;
      R_text(menu_line) ;
      R_flush() ;
      }
    unset_keyboard (); 

    Dchoose(MEN.name);
    erase_popup("ques");

    ptr = menu_line;
/*#ifdef SYSV
	ptr+=3;
#else*/

	ptr+=2;

/*#endif*/

    while (1)
      {
      *(answer) = *(ptr);
      if (*ptr == '\0') break;
      answer++; ptr++;
      }
}
