.bp
.HU "Appendix A - graphics.h"
.B Graphics.h
is required to resolve symbols mentioned in the preceding
text and should be included with all software using
the graphics system.
Note that there are several definitions at the end of the file
which make graphics code much more readable.
.DS
.ta 4 +4 +4 +4 +4 +4 +4 +4 +4 +4 +4 +4 +4 +4 +4 +4 +4 +4
.so graphics.h
.DE
.bp
.HU "Appendix B - grerror.h"
.B Grerror.h
is employed by
.B plotter
to define values to be associated with detected errors in
execution of the graphics system.
.DS
.so grerror.h
.DE
.bp
.HU "Appendix C - example program"
This is a sample program employed to generate listings of
font tables and demonstrates some of the graphics system entries.
For example:
.DS
	ftab -sr +s .25
.DE
will cause the standard font
.B sr
to be displayed on the controlling teletype terminal.
All arguments after the first are passed to the program
.B plotter.
In this case, the output was scaled by .25.
.DS
.so tests/ftab.c
.DE
.bp
.HU "Appendix D - example cursor input program"
A sample program demonstating cursor requests and line drafting.
To execute (on an interactive graphics terminal):
.DS
	ut2
.DE
The screen should be cleared and the cursor (or cross hairs) should
appear.
Position cursor and enter either a
.B N
or a
.B C.
A symbol will be posted and the cursor will reappear.
After repositioning the cursor type a blank (space bar)
and a line will be drawn from the last point to the current position
and a symbol plotted.
To discontinue the demonstration, type a
.B D.
.DS
.so tests/ut2.c
.DE
.bp
.HU "Appendix E - Makefile for graphics system"
.P
The following is a listing of the curret working version of the UNIX V
makefile for generation of the graphics system:
.DS
.so driver/Makefile
.DE
.bp
.HU "Appendix F - UNIX manual sections"
Following pages are reproductions of documentation consistent with
.B man
format which provide a synopsis of graphic system software.
.bp
.HU "Appendix G - font tables"
.P
The following pages contain reproductions of the output of program
.B ftab
which illustrate the several fonts generated as part of
the distributed software.
