#include <stdio.h>
#include "X11/Xlib.h"
#include "X11/Xutil.h"
#include "gis.h"
#include <X11/IntrinsicP.h> 
#include <X11/Intrinsic.h> 
#include <X11/Shell.h>
#include <X11/Core.h>
#include <X11/Atoms.h>
#include <X11/AsciiText.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include <X11/Simple.h>
#include <X11/Composite.h>
#include <X11/Form.h>

#define MAX(x,y) x>y ? x : y
extern Widget toplevel;
extern XtTranslations map_translations;
extern Arg form_args[];

struct Cell_head gis_window;

static int nrows, ncols;

char *mapset;

CELL *cell;
static num_displayed_maps=0;
static struct pix_struct{ 
      Pixmap pixmap;
      String pixmap_name;
    } pixmap_struct[10];

Widget create_map(map_name)
  char *map_name;
{
  Window the_window;
  int the_screen;

  char   buf[100];

  Widget map_widget, 
	txt_widget,
         map_shell;

  XFontStruct *font;
  Cursor the_cursor;
  
  Arg args[10];
  int n;
 
  int nrows, ncols,
      display_rows, display_cols,
      max_display_rows, max_display_cols,
      width_ratio, height_ratio, aspect_ratio,
      width, height,
      display_width, display_height;


  XEvent *event;


  /* the gis part */
       G_gisinit("Xcell");
        G_get_window(&gis_window);

       mapset =  G_find_cell2(map_name, "");

       if(mapset == NULL)
       {
          sprintf(buf,"Cellfile [%s] not available", map_name);
          G_fatal_error(buf);
       }

       nrows = G_window_rows();
       ncols = G_window_cols();

       cell = G_allocate_cell_buf(); /* end of gis part */

       the_screen = DefaultScreen(XtDisplay(toplevel));

   /* root_window = RootWindow(the_display,the_screen);*/

      display_width = XDisplayWidth(XtDisplay(toplevel), the_screen);
      display_height = XDisplayHeight(XtDisplay(toplevel), the_screen);

    if ((display_width/ncols < 4) || (display_height/nrows < 4))
      {
        max_display_cols = display_width/4;
        width_ratio = ncols/max_display_cols +1;

        max_display_rows = display_height/4;
        height_ratio = nrows/max_display_rows + 1;

        aspect_ratio = MAX(width_ratio, height_ratio);

        display_cols = ncols/aspect_ratio;
        display_rows = nrows/aspect_ratio;
      }
    else
      {
        display_cols = ncols;
        display_rows = nrows;
      }

	
       XtSetArg(args[0], XtNminAspectX, display_cols);
       XtSetArg(args[1], XtNminAspectY, display_rows);
       XtSetArg(args[2], XtNmaxAspectX, display_cols);
       XtSetArg(args[3], XtNmaxAspectY, display_rows);
       XtSetArg(args[4], XtNwidthInc,  display_cols);
       XtSetArg(args[5], XtNheightInc, display_rows);
       XtSetArg(args[6], XtNtitle, map_name);	
      

       map_shell = XtCreatePopupShell(map_name, 
				topLevelShellWidgetClass,
                                   toplevel ,args, 7);


      /* if (QLength(XtDisplay(toplevel))>0)
         {
           XPeekEvent(XtDisplay(toplevel), event);
           if (event->type == ConfigureNotify);
              {
                XNextEvent(XtDisplay(toplevel), event);
            width = (int)((XConfigureEvent *)event)->width;
            height = (int)((XConfigureEvent *)event)->height;
               }
           }
        else 
          {
             width = display_cols;
             height = display_rows;
          }*/
       XtSetArg(args[0], XtNwidth,  2*display_cols);
       XtSetArg(args[1], XtNheight, 2*display_rows);
       XtSetArg(args[2], XtNtitle,  map_name);

       map_widget = XtCreateManagedWidget(map_name, compositeWidgetClass,map_shell,
                                          args, 3);
 
       XtOverrideTranslations(map_widget, map_translations);

        font = XLoadQueryFont(XtDisplay(toplevel), "6x10.snf");
        the_cursor = XCreateFontCursor(XtDisplay(toplevel),62);

        n=0;
        XtSetArg(args[n], XtNfont, font); n++;
        XtSetArg(args[n], XtNcursor, the_cursor); n++;
        XtSetArg(args[n], XtNwidth, 102); n++;
        XtSetArg(args[n], XtNheight, 14); n++;
	XtSetArg(args[n], XtNhorizDistance, 0); n++;
	XtSetArg(args[n], XtNfromHoriz, NULL); n++;
	XtSetArg(args[n], XtNvertDistance, 0); n++;
	XtSetArg(args[n], XtNfromVert, NULL); n++;
	XtSetArg(args[n], XtNresizable, FALSE); n++;

        txt_widget = XtCreateManagedWidget("coors", widgetClass,
                        map_widget, args, n);

       XtPopup(map_shell, XtGrabExclusive);

   
       /* process_map(map_widget, event); */
}     







process_map(w,event)
	Widget w;
        XEvent *event;
{
 

GC the_GC;

XGCValues values;


int width, height,
    row,
    old_col, 
    a, b,
    iold,
    line_width,
    col_inc, row_inc,
    fd,
    count=0,
    rect_x, rect_y;
	static int  cunt = 0;

int the_screen;

double width_fac, height_fac;

char buf[100];

Window the_window;
Display *the_display;

register int i;


           /* printf("event %d",((XConfigureEvent *)event)->type;
           exit();  */
	
	
/*
           if (event->type == ConfigureNotify)
           {
           width = (int)((XConfigureEvent *)event)->width;
           height = (int)((XConfigureEvent *)event)->height;
           }
*/
	width = (int) (w->core.width);
	height = (int) (w->core.height);

            /* exit(); */
         /*  if(width == old_width && height == old_height)
             continue;

           old_width = width;
           old_height = height;*/

           nrows =   G_window_rows();
           ncols = G_window_cols();

           line_width = (int)(height/nrows);
           if(line_width < 1)
              line_width = 1;

           values.line_width = line_width;
           values.foreground = 1;
           the_GC = XtGetGC(w,GCLineWidth|GCForeground,&values);

           the_display = XtDisplay(toplevel);
           the_window = XtWindow(w);
           the_screen = DefaultScreen(the_display);
 
           XDrawString(XtDisplay(w), XtWindow(w), the_GC,
                       width/2-30,height-15,
                       "Displaying, please wait...",26);

/*           if(pixmap != 0)  XFreePixmap(*the_display,pixmap);*/

           fd = G_open_cell_old(w->core.name,mapset);
           if (fd < 0)
             {
               sprintf (buf,
                 "%s in %s -can't open cell file",
                 w->core.name,
                 mapset);
               G_fatal_error (buf);
             }


            width_fac = (float)width/ncols;
            height_fac = (float)height/nrows;

            col_inc = ncols/width;
            if(col_inc<1)
              col_inc = 1;

            row_inc = nrows/height;
            if(row_inc<1)
              row_inc = 1;


        rect_y = 0;

             while(count<num_displayed_maps)
             {
               if(w->core.name == pixmap_struct[count].pixmap_name)
                  {
                    XFreePixmap(the_display, pixmap_struct[count].pixmap);
                    break;
                   }
               count++; 
             }

             pixmap_struct[count].pixmap_name = w->core.name;

             if (count>=num_displayed_maps) num_displayed_maps++;
           pixmap_struct[count].pixmap = XCreatePixmap(the_display,
				 DefaultRootWindow(the_display),
				 width, height,8);

            for(row=0; row<nrows ; row += row_inc)
              {
                if(G_get_map_row(fd, cell, row) < 0)
                  exit();

                i = 0; iold =0;

                rect_x = 0;


             while(i<ncols)
               {
                 while (i<(ncols-1))
                   {
                     if(*(cell+i) != *(cell+i+col_inc)) break;
                     i += col_inc;
                   }


                 XSetForeground(the_display,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      