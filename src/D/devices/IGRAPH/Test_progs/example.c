#include <tools.h>

#define MYEVENTS (REFRESH_EVENT | BUTTON_EVENT | KEYBOARD_EVENT | DELETE_EVENT)

main()
{
	int	vsno;		/* Number of active virtual screen */
	int	wno;		/* Window number */
	int	wxl = 100,
		wyl = 100,
		wxh = 600,
		wyh = 600;	/* Initial working area of window */

	int	axl, ayl,
		axh, ayh;		/* Area that requires update */
		
	int	cx = 250,
		cy = 250;	/* Center of "X" */
	
	int	curevents;	/* Mask of outstanding events */
	char	keybuf;		/* Buffer for keyboard data */
	int	keycnt;		/* Number of characters received */
	int	tmp;		/* Dummy variable... stuff I don't care about */

	/* Perform initialization and activate process */
	Enter_tools();

	/* Set process logo */
	Set_logo ("XX");
	
	/* Determine the number of the active virtual screen */
	Inq_displayed_vs (&vsno);
	
	/* Create window */
	Create_win (vsno, "Warped X", wxl, wyl, wxh, wyh, &wno);
	
	/* Disable manual refresh */
	Set_win_sys_icon (wno, REFRESH_ICON_VALUE, 0);
	
	/* Display the window */
	Display_win (wno);
	
	/* Draw "X" in window */
	drawX (wno, cx, cy, wxh - wxl, wyh - wyl);
	
	/* Initialize keyboard buffer */
	Set_keyboard_buffer (&keybuf, sizeof (keybuf));
	
	/* Enable refresh, button, delete, and keyboard events */
	Enable_events (MYEVENTS);
	
	/* Begin idle loop */
	for (;;)
	{
		/* Wait for something interesting to happen */
		Wait_for_events (MYEVENTS, &curevents);		

		/* Was the window deleted? */
		if (curevents & DELETE_EVENT)
		{
			/* I know which window it was, so
			   just clear the event */
			   
			Clear_delete_data ();
			Exit_tools ();
			exit(0);
		}
		
		/* Does the window need updating? */
		if (curevents & REFRESH_EVENT)
		{
			/* I don't really need to know the window number,
			   virtual screen number, or the opmask. */
			Get_refresh_area_data(&tmp,&tmp,
				&wxl,&wyl,&wxh,&wyh,
				&axl,&ayl,&axh,&ayh, &tmp);

			/* Set the clipbox to the area to be refreshed so
			   unnecessary graphics won't be drawn. */
			clipbox (wno, axl, ayl, axh, ayh);
			drawX (wno, cx, cy, wxh - wxl, wyh - wyl);
		}

		/* Character typed on keyboard? */
		if (curevents & KEYBOARD_EVENT)
		{
			/* keycnt should == 1 since keybuf is one byte */
			Get_keyboard_data(&keycnt);
			if (keybuf == 'q' || keybuf == 'Q')
			{
				/* Exit_tools will clean up windows */
				Exit_tools();
				exit(0);
			}
		}

		/* Button pressed on pointing device? */
		if (curevents & BUTTON_EVENT) 
		{
			/* Don't really care which button, what type... etc.
			   I just want the position. */
			Get_button_data (&tmp, &cx, &cy, &tmp, &tmp, &tmp);

			/* Set clipbox to enclose working area */
			clipbox (wno, 0, 0, wxh - wxl, wyh - wyl);

			/* draw X */
			drawX (wno, cx, cy, wxh - wxl, wyh - wyl);
		}
	}
}
			
drawX (wno, x, y, sizex, sizey)
int	wno;			/* Window to draw "X" in */
int	x, y;			/* Center of "X" */
int	sizex, sizey;		/* Size of window */
{
	/* Must undisplay the cursor before issuing graphics commands */
	hidecursor (wno);

	/* Erase contents of the window, use color index 0 */
	fgcolor (wno, 0);
	rectf (wno, 0, 0, sizex, sizey);
	
	/* Draw from each corner of the window to the "center" */
	/* Use whatever color is at index 1 */
	fgcolor (wno, 1);	
	move (wno, 0, 0);
	draw (wno, x, y);
	move (wno, sizex, 0);
	draw (wno, x, y);
	move (wno, sizex, sizey);
	draw (wno, x, y);
	move (wno, 0, sizey);
	draw (wno, x, y);

	/* Display cursor again */
	showcursor (wno);
	
	/* Must flush graphics buffer to ensure screen is up to date */
	flushbuffer (wno);
} 
  
