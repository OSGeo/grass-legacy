/*
 * Using mouse device, get a new screen coordinate and button number.
 * Button numbers must be the following values which correspond to the
 * following software meanings:
 *   1 - left button
 *   2 - middle button
 *   3 - right button
 *
 * This is called directly by the application programs.
 *
 * A "rubberband" box is used.  One corner is fixed at the (cx, cy) coordinate.
 * The opposite coordinate starts out at (*wx, *wy) and then tracks the mouse.
 * Upon button depression, the current coordinate is returned in (*wx, *wy) and
 * the button pressed in returned in *button.
 */

#include "graphics.h"

#define pw_box(pixwin, x0, y0, x1, y1, brush, tex, op) \
        pw_vector(pixwin, x0, y0, x0, y1, op, 1); \
        pw_vector(pixwin, x0, y1, x1, y1, op, 1); \
        pw_vector(pixwin, x1, y1, x1, y0, op, 1); \
        pw_vector(pixwin, x1, y0, x0, y0, op, 1) 

Get_location_with_box(cx, cy, nx, ny, button)
        int cx, cy ;    /* current x and y */
        int *nx, *ny ;  /* new x and y */
        int *button ;
{
        Event event;
        Event *pevent = &event;
        int     ox, oy;

        window_set(base_canvas, WIN_CONSUME_PICK_EVENTS,
                WIN_NO_EVENTS, WIN_MOUSE_BUTTONS, LOC_MOVE, LOC_DRAG, 0,
                0);
        ox = cx;
        oy = cy;
        while(1)
          {
                window_read_event(base_canvas, pevent);
                if( (event_id(pevent) == LOC_MOVE) ||
                        (event_id(pevent) == LOC_DRAG) ||
                        (event_id(pevent) == KBD_USE))
                  {
                        /* erase old line */
                        pw_box(pixwin, cx, cy, ox, oy,
                                0, NULL, PIX_COLOR(255) | (PIX_SRC^PIX_DST));
                        ox = event_x(pevent);
                        oy = event_y(pevent);
                        /* draw new line */
                        pw_box(pixwin, cx, cy, ox, oy,
                                0, NULL, PIX_COLOR(255) | (PIX_SRC^PIX_DST));
                  }
                else if(event_is_button(pevent) && (event_is_down(pevent)))
                  break;
          }
        /* erase old line */
        pw_box(pixwin, cx, cy, ox, oy,
                0, NULL, PIX_COLOR(255) | (PIX_SRC^PIX_DST));
        window_set(base_canvas, WIN_CONSUME_PICK_EVENTS,
                WIN_NO_EVENTS, WIN_MOUSE_BUTTONS, 0,
                0);

        *button = event_id(pevent) - BUT(0);
        *nx = event_x(pevent);
        *ny = event_y(pevent);
}
  
