.bp
.HU Introduction
This system is designed to provide primative graphics capabilities
for 'C' applications programs which are independent of the actual
graphics device employed.
In addition, considerable effort was made to provide the utmost in flexibility
and transportability of code.
This document, however, is only designed as a reference manual and
not a complete description of potential usage.
.P
Basically, this system consists of a set of functions executed by
the applications program which create a 'meta-graphic' stream to control
a graphics device.
This meta-graphic stream can be either directly piped to the program
.B plotter
which actually controls the graphics device or saved in a file
for later processing by
.B plotter.
The applications programmer is not normally concerned with the
peculiarities of the actual device being employed.
.P
It is assumed that each
.B C
file refering to function entries contained in this
document will employ
.DS
# include <graphics.h>
.DE
to resolve declarations contained in the following text.
In addition, it is advisable for the user to scan this
(see Appendix A) file
and be aware of some of the undocumented macros which
can facilitate usage.
.P
Lastly, this system expects only the most primative of
graphic devices.
Selection of multiple mechanical pens when available,
pen motion, and up-down (no-draft/draft) actions.
Capability of cursor coordinates is also provided
for interactive devices.
Other sophisticated features,
which vary widely with various manufacturers and models,
are ignored with this system in the interests of
portability.
