                   Geographics Digitizer Information

Documentation on cabling configuration needed for geographics.
There is also a diagnostics program under
$GISBASE/src/mapdev/digitizers/geograph/Diagnostics.


Cabling set-up for Geographics digitizer.
RS232 pins needed:  2, 3, 7.   all straight through.


	Pin		Meaning
	---		-------
	2		Transmit data
	3		Receive data
	7		Signal ground

	6		Data set ready
	8		Received line signal detector
	20		Data terminal ready

*************************************************************************
*  For MASSCOMPs only.   Others may vary.
*  Besides having pins 2, 3, and 7 straight through:
*
*  The newer, slimmer, smaller serial box has to also have 6, 8, and 20
*  looped back in the cable or on the masscomp end.
*
*  The old very large serial box has 8, and 20 straight through.
*
*************************************************************************

The /etc/inittab file must have the getty off for the digitizer port (tty).

