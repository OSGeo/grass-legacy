.bp
.H 1 "Installation"
.P
The basic distribution of this graphics package allows
for loading at any installation selectable directory path.
The following script illustrates a typical loading of the
distribution
.B cpio
tape:
.DS 5
cd /u/me
mkdir graph
cd graph
cpio -icBvdm </dev/rmt
.DE
For this installation the root directory of the graphics
system becomes /u/me/graph.
.H 2 "Environment setup"
.P
Once the tape is loaded the installer should examine the file
.B set_the_env
in the subdirectory sources:
.DS
# sample csh environment setting script
setenv GRAPH /usr/graph
setenv GRAPHB /usr/graph/plotter:/usr/graph/fonts/:sr:hp7475,/dev/ttym2:
.DE
The GRAPH entry establishes the root of the graphics system and
is of primary use by the applications programmer when referring
to the location of the library and the user when referring
to program plotter.
Entry GRAPHB is employed by the library routines as well as
program plotter to establish the characteristics of the
local installation.
This file should be edited to local needs and included as
part of the user login procedures.
.P
Using the previous loading path the first entry should be
changed to:
.DS 5
setenv GRAPH /u/me/graph
.DE
The second entry, GRAPHB, consists of several fields separated
by :'s.
The first field is the full path name for program plotter, the
second is the path of the standard font directory (followed by the /), and
the third is the font to be employed as a default (sr in this case).
As per the sample loading, the new GRAPHB entry should look like:
.DS
setenv GRAPHB /u/me/graph/plotter:/u/me/graph/fonts/:sr:hp7475,/dev/ttym2:
.DE
.SP
Once set_the_env has been appropriately edited do a
.DS 5
source set_the_env
env
.DE
.SP
to check it out and make the environment variables available
before continuing.
.P
The remaining fields refer to non-terminal type plotters and
consist of the plotter name, a comma and the name of the output port
associated with the device.
In this case the Hewlett Packard 7475 is attached to tty port /dev/ttym2.
.P
If the identifiers GRAPH and GRAPHB interfere with other
local system environment names they can be changed provided care is exercised
in providing the replacement for GRAPHB when executing
driver/Makefile.
.P
An important part of interactive use of this graphics system is
the use of the TERM environment variable which allows automatic selection
of appropriate output driver when not explicitly specified.
This parameter is normally established in the user
.B .login
file as part of establishing terminal characteristics.
Because of frequent local variations of terminal names
the installer must edit the file
.B devlist.c
to reflect the equivalency of local TERM identifiers with
internal plotting drivers.
For testing purposes the installer can change the value of
TERM to one of the current entries.
Multiple name entries can, of course, refer to the same driver: this
is merely a mapping table.
.P
In situations where the using community is directly executing program
.B plotter
a copy of the program may either be placed in a library common
to most search paths (ie. /usr/local/bin) or establish an appropriate alias.
.H 2 "Font table generation"
The next step is to generate the standard font library directory and
files using the
name in the GRAPHB environment (normally
.B fonts).
Execute:
.DS
makefonts fonts
.DE
.SP
The script file makefonts uses the argument
.I fonts
us the name of the standard font directory.
.H 2 "Plotter and library installation"
.P
Change directory to
.B driver
and examine the file
.B Makefile
for additional installation notes.
If some plotter drivers are not needed they may
be edited out of the
.B DEVS.o
list and the
.B devlist.c
module.
Unresolved loader references can be ignored on some systems
but it is wise
to eliminate these entries from the driver structure in
.B plotter.c
to avoid crashes when an unresolve driver is named.
Load software by:
.DS
make system
.DE
.P
Because of significant differences between the Berkeley 4.x UNIX
and UNIX V
.B make
program an alternative makefile file is provided:
.B BSDmakefile.
.H 2 "Testing installation"
.P
Change directory to
.B ../tests
and do a
.DS
make ftab
ftab - -s .25
.DE
.SP
Since the scaling is fixed in the program to a 4014 screen
the -s .25 is necessary when the terminal is a Tektronic
4010 or clone (ie. 0 to 1023 x-axis resolution).
If all has gone well the standard default font should be
displayed.
.P
Note the methods in the
.B makefile
for handling the compilation process of software using
the graphics system.
.H 2 "Cleanup"
Once the system has been checked out and verified as operational
backup the changes.
The directory (and sub directories)
.B sources
can now be removed if there are space limitations.
