README.make --- Remarks on the installation with the generic Makefile.

The Makefile runs Makefile.cpp (and Makefile.sys) through the C
preprocessor to create Makefile.tmp.  It is this file that is then
executed with the make command.  This approach leads to somewhat
portable makefiles by taking advantage of the "#ifdef <machine>"
commands of the C pre-processor.


HOW TO INSTALL:

Modify Makefile.cpp to determine the desired target directories, 
etc, and edit Makefile.sys according to the needs of your system.
Then run:

        make -k clear normal

To compile a dbx'able version run:

        make -k clear debug


(( If you port this code to systems not listed below,  please  let
   me know.  My email-address is at the end of this file.  Thanks. ))


SUPPORTED SYSTEMS             | C pre-processor commands
------------------------------+-------------------------
Silicon Graphics Irix 4.0.1   | #ifdef sgi
SUN 3, 4, SunOS 4.1.1            | "default" or #ifdef sun
SUN SPARC                     | #ifdef sparc
Convex                        | #ifdef __convex__             (?)
NeXT                          | #ifdef NeXT
IBM RS/6000                   | #ifdef _IBMR2


NOTES/BUGS:

AD lint: If you use the additional heuristics, lint warns about
"constant in conditional context" (since things like "while(0)" are
used) and some "possible pointer alignment problems" and may be some
other things.  Ignore them, or turn heuristics off.

AD lint on Sun SPARCs: It gives tons of warnings, esp, "possible
pointer alignment problems" in/with basic/malloc.c...  :-(

Good luck,
         _
--Ernst Mucke   <mucke@uiuc.edu>
  Dept of Computer Science, UIUC
