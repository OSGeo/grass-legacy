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
 * A pointer is used. This can be a crosshair, pointer, or cursor. 
 * It starts out at (*wx, *wy) and then tracks the mouse.
 * Upon button depression, the current coordinate is returned in (*wx, *wy) and
 * the button pressed in returned in *button.
 */

#include "graphics.h"

Get_location_with_pointer(wx, wy, button)
        int *wx, *wy ;
        int *button ;
{
        Event event;
        Event *pevent = &event;
        /* printf("waiting for mousedown..."); fflush(stdout); */
        while(1)
          {
                window_read_event(base_canvas, pevent);
                if(event_is_button(pevent) && (event_is_down(pevent)))
                  break;
          }
        *button = event_id(pevent) - BUT(0);
        *wx = event_x(pevent);
        *wy = event_y(pevent);
        /* printf("got button %d (%d, %d)\n", *button, *wx, *wy); */
}
