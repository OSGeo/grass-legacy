


m.examine.tape <main> GRASS Reference Manual<main> m.examine.tape



NAME
     m.examine.tape - Provides a description of	the files on a
     1/2-inch magnetic tape.
     (GRASS File Management Tool)

SYNOPSIS
     m.examine.tape

DESCRIPTION
     m.examine.tape is a convenient tool for previewing	a tape
     and obtaining block size values.  After the user enters the
     command

	  m.examine.tape

     the program will prompt the user for the full path	name of
     the tape device on	which the tape is mounted.  In most
     cases, this will be /dev/rmt0.  However, the user must enter
     this, as there is no default value	for the	tape device name.
     The user is then prompted to enter	the buffer size	to be
     used when reading the tape.  The buffer size is the amount
     of	memory allocated to read one physical record of	the tape.
     If	the user hits RETURN, the program will assume a	default
     buffer size of 32767 bytes.

     m.examine.tape reads the tape specified by	the user and
     reports back the block size (record length) and the number
     of	blocks for each	file contained on the tape.
     m.examine.tape can	be used	on any 1/2-inch	magnetic tape.
     The user has the option of	sending	the output into	a file or
     viewing the output	on the terminal	screen.

NOTES
     The buffer	size is	the amount of memory allocated to read
     one physical record on the	tape.  Magnetic	tape devices will
     accept a request to read more bytes than actually exist in
     any given record on a tape.  However, m.examine.tape reads
     only as many bytes	as physically exist on the tape	and
     returns the number	of bytes actually read.	 The user is
     allowed to	alter the buffer size in order to request smaller
     reads for tape devices unable to handle requests this large
     (32767 bytes), or to request larger reads for tapes with
     larger record sizes read on drives	able to	handle larger
     record sizes.

     Note that m.examine.tape cannot be	used to	examine	the
     content of	either ordinary	files or 1/4-inch tape
     cartridges.

SEE ALSO
     m.dem.examine, m.dem.extract, m.dmaUSGSread, m.dted.examine,
     m.dted.extract, m.lulc.USGS, m.lulc.read



GRASS 4.2		Baylor University			1






m.examine.tape <main> GRASS Reference Manual<main> m.examine.tape



AUTHOR
     Michael Shapiro, U.S. Army	Construction Engineering Research
     Laboratory




















































GRASS 4.2		Baylor University			2



