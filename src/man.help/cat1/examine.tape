


examine.tape(1)      GRASS Reference Manual       examine.tape(1)



NAME
     examine.tape - Provides a description of the files on a 1/2
     inch magnetic tape
     (Available through grass3 menu)

SYNOPSIS
     examine.tape

DESCRIPTION
     examine.tape is a convenient tool for previewing a tape and
     obtaining block size values.  examine.tape will prompt the
     user for the tape device path.  There are no default values
     for this entry;  in most cases, it will be /dev/rmt0.
     examine.tape reads the tape specified by the user and
     reports back the block size and the number of blocks for
     each file contained on the tape.  examine.tape can be used
     on any 1/2 inch magnetic tape.  The user has the option of
     storing the output into a file or viewing the output on the
     screen.

NOTES
     Note that examine.tape cannot be used for ordinary files or
     1/4 inch cartridges.

AUTHOR
     Michael Shapiro, U.S. Army Construction Engineering Research
     Laboratory




























GRASS 3.2                U.S. Army CERL                         1



