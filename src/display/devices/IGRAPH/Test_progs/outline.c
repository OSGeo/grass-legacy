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
	Create_win (vsno, "TEST DRIVER X", 0, 0, MAX_SCREEN_WIDTH, MAX_SCREEN_HEIGHT, &wno);
	
	/* Disable manual refresh */
	Set_win_sys_icon (wno, REFRESH_ICON_VALUE, 0);
	
	/* Display the window */
	Display_win (wno);
	
	drawit2 (wno);

	sleep(10) ;
	
	/* Exit_tools will clean up windows */
	Exit_tools();
	exit(0);

}
			
drawit1 (wno)
int	wno;			/* Window to draw "X" in */
{
	/* Must undisplay the cursor before issuing graphics commands */
	hidecursor (wno);

	/* Erase contents of the window, use color index 0 */
	fgcolor (wno, 9);
	rectf (wno, 0, 0, MAX_SCREEN_WIDTH - 80, MAX_SCREEN_HEIGHT - 80);
	
	/* Draw from each corner of the window to the "center" */
	/* Use whatever color is at index 1 */
	fgcolor (wno, 8);	
	weight( wno, 30) ;
	move (wno, 0, 0);
	draw (wno, MAX_SCREEN_WIDTH -400, MAX_SCREEN_HEIGHT -400);

	weight( wno, 0) ;

	move (wno, MAX_SCREEN_WIDTH -400, MAX_SCREEN_HEIGHT);
	draw (wno, MAX_SCREEN_WIDTH -400, MAX_SCREEN_HEIGHT -400);

	move (wno, MAX_SCREEN_WIDTH , MAX_SCREEN_HEIGHT -400);
	draw (wno, MAX_SCREEN_WIDTH -400, MAX_SCREEN_HEIGHT -400);


    /*  draw box  */
	move (wno, MAX_SCREEN_WIDTH/2 , MAX_SCREEN_HEIGHT/2);
	draw (wno, MAX_SCREEN_WIDTH/2+1 , MAX_SCREEN_HEIGHT/2);

	draw (wno, MAX_SCREEN_WIDTH/2+1 , MAX_SCREEN_HEIGHT/2+1);
	draw (wno, MAX_SCREEN_WIDTH/2+1 , MAX_SCREEN_HEIGHT/2);
	draw (wno, MAX_SCREEN_WIDTH/2 , MAX_SCREEN_HEIGHT/2);

	/* Display cursor again */
	showcursor (wno);
	
	/* Must flush graphics buffer to ensure screen is up to date */
	flushbuffer (wno);
} 
  
			
drawit2 (wno)
int	wno;
{
	/* Must undisplay the cursor before issuing graphics commands */
	hidecursor (wno);

	fgcolor (wno, 8);
	rectf (wno, 0, 0, MAX_SCREEN_WIDTH , MAX_SCREEN_HEIGHT);
	flushbuffer (wno);

	fgcolor (wno, 9);
	rectf (wno, 1, 1, MAX_SCREEN_WIDTH-1 , MAX_SCREEN_HEIGHT-1);

	/* Display cursor again */
	showcursor (wno);
	
	/* Must flush graphics buffer to ensure screen is up to date */
	flushbuffer (wno);
} 
  
