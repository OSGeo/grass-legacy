.bp
.H 1 "Driver implementation"
.P
A short discussion will be given here for programmers
involved in modifying
.B plotter
for new graphics devices.
This operation is made as painless as possible by having
all data processed by
.B plotter
passed to the device dependent software through
one entry point with a few command options.
.P
First the device driver must be declared as a function returning a pointer
to a structure defined by
.B XYS
defined in
.B plotter.h.
Note that a legitimate pointer return value is only required by the
.B "cmd = D_INIT, D_SCALE"
and
.B D_CURSOR
entries.
The number of elements in the argument list, as well as the type
of the third argument, are
dependent upon the subsequently described values of 
.B cmd.
.VL 10
.LI D_INIT
This is the first command call to the driver made by
.B plotter.
The third argument is a pointer to an ascii string defining the name
of the file for output of the data stream to drive the plotting
device.
If the string is null (first character = '\\0') then the driver
is expected to take some default action.
In the case of the 4014 (and most interactive devices)
the default output is directed to
.B stdout.
If the driver routine cannot open the designated file then
it should return a
.B NULL
pointer sensed by
.B plotter
as abnormal an condition.
Upon successful output file handling, the contents of the local 
.B XYS
structure's x-y should be loaded with the maximum size of the
device and a pointer to this structure returned.
.LI D_SCALE
The scaling entry passes the applications programs world to device
scaling requirements as a type double float
third argument to the driver as a multiplicative factor applied
to all subsequent coordinate input data.
If the factor is <= 0, the driver should set the local copy of
the scaling factor to 1.
The driver must return updated values for the
size new size of the device as per
.B D_INIT.
.LI D_DONE
When the applications program executes a
.B plotend
call, the graphics operation is considered terminated.
Prior to termination of the
.B plotter
process, a
.B D_DONE
call is made to the driver so that any device dependent
cleanup work may be performed.
Note that it usually desirable to send a bell character
to interactive devices and wait for some user response so that hard
copy operations can be made before the display is cluttered with system
prompts or other material.
.LI D_DISABLE
Some interactive graphics devices may require
a shift out of graphics mode in order to perform non-graphics
operations.
This entry command signals that such a mode change should be made.
Any other graphics operation will expect an automatic shift
out of this disabled state upon entry.
In the particular case of the Tektronixs storage tube devices,
it is also desirable to shift out of graphics mode when
there are long delays in display activity in order to help
lengthen the life of the display tube.
.LI D_ERASE
This command entry expects that all graphics artifacts on the
display be removed.
It obviously only applies to non-hard copy devices not already
written on.
.LI D_PEN
The third (type long)
argument contains a value dependent upon the graphics device.
For example, if the device has several mechanical pens,
it may indicate which pen is to be selected for the next
graphical operations.
In the case of the 4014 driver it is ignored.
.LI D_MOVE
The second and third arguments (type long) are the respective
x and y world coordinates at which the pen is to positioned without
drafting a line.
.LI D_LINE
This is the same as
.B D_MOVE 
except that a line should be drafted between the previous
position of the pen and the new coordinates.
.LI D_CURSOR
If the device has cursor input capability a pointer to
an XYS structure is returned
whose x-y contents have been loaded with
world coordinates of the cursor.
Any ancillary character stream peculiar to the device
will be stored in the
.B s
string portion of the structure.
In the case of the 4014 a keyboard character is returned
when the cursor position is transmitted.
For non-cursor devices the driver should return a
.B NULL
pointer.
.LE
.P
Two external integer variables are set by the plotting system
for use by the driver:
.B reverse
and
.B drive_opt.
The value of
.B reverse
is set non-zero if the
.B -r
runline option is specified (axis reversal).
If possible,
the driver software should rotate the graphic on
the plotting device by 90\(de.
This option is normally employed to secure maximum plotting
size when the current x-y size ratio of the plot region
is more appropriate to a rotated presentation on the physical device.
.P
.B Drive_opt
is optained from the
.B dev_list
structure in module
.B devlist.c.
Its usage is intended to allow devices with similar plotting
logic to use the same basic source code.
Variations of the value of
.B drive_opt
can then be sensed by a more general driver to vary output or
control of a specific device.
Examination of the source code for the Tektronics driver,
.B g4014.c,
will show how several bit fields allow for specification of
special variants among 401x clones.
.P
In conjunction with installing a new device, the
.B plotter
program module file
.B devlist.c
structure
.B dev_list
must be updated with the ascii identification of the new
device and a pointer to the entry point.
Comments in
.B devlist.c
and
.B plotter.h
should be sufficient for explantion of structure entries.
