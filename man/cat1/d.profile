


d.profile <main>     GRASS Reference Manual	 <main>	d.profile



NAME
     d.profile - Displays profiles of a	user-specified raster map
     layer.
     (GRASS Display Program)

SYNOPSIS
     d.profile

DESCRIPTION
     This command works	only interactively.  It	clears the entire
     graphics screen and provides a graphical interaction
     allowing the selection of transects for which profiles are
     then generated.

USER PROMPTS
     First, you	will be	presented with a prompt	asking you to
     choose a raster map layer to be profiled.	Once you specify
     a valid raster map	layer name, the	map layer will be
     displayed in the left half	of the graphics	display, and the
     right half	of the dispay will be divided into four	display
     frames.  There will also be two frames along the top of the
     display:  a mouse-button menu frame on the	left, and a
     status frame on the right.

     The mouse-button menu initially offers you	three options:

	  Mouse	Buttons:
	     Left:	  Where	am I?
	     Middle:	  Mark FIRST Point of Profile Line.
	     Right:	  QUIT this.

     You may query the displayed raster	map layer by indicating
     points with the left mouse-button.	 The coordinates and
     category value of each point that you indicate will be
     displayed on in the status	frame.	If you mark the	first
     point of the profile line you will	be presented with the
     following mouse-button menu:

	  Mouse	Buttons:
	     Left:	  Where	am I?
	     Middle:	  Mark SECOND Point of Profile Line.
	     Right:	  QUIT this.

     Once you mark the second point of the profile line, the
     profile line will be labeled (with	a letter from A	to D) and
     displayed in one of the four display frames on the	right
     hand side of the screen.  You will	then be	presented with a
     third mouse-button	menu:

	  Mouse	Buttons:
	     Left:	  DO ANOTHER
	     Middle:	  CLEAR	DISPLAY



GRASS 4.2		Baylor University			1






d.profile <main>     GRASS Reference Manual	 <main>	d.profile



	     Right:	  QUIT this.

     If	you would like to view another profile,	click on the left
     mouse-button.  If you would like to redisplay the raster map
     layer and clear out the four profile frames, click	on the
     middle mouse-button.  If you would	like to	quit, then click
     on	the right button.

NOTES
     Useful enhancements to d.profile would include:

     1)	Adding an option to display profiles using category
     colors, like a bar-chart.
     2)	Allowing profile lines to be defined by	a series of
     points, not just two.
     3)	Allowing profiles to be	saved in a file, for later
     viewing.
     4)	Allowing the user to enter profile line	points by typing
     coordinates.

AUTHOR
     Dave Johnson
     DBA Systems, Inc.
     10560 Arrowhead Drive
     Fairfax, Virginia 22030






























GRASS 4.2		Baylor University			2



