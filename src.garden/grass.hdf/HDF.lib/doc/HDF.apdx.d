D.1	NCSA HDF Calling Interfaces and Utilities

Public HDF Directories on NCSA Computers	D.1

National Center for Supercomputing Applications

July 1990

                                                                

July 1990




Appendix  D	Public HDF Directories on NCSA Computers




There are public HDF directories on several machines at NCSA. 
In each supported HDF directory you will find the following:

¥	A README file that gives further information about the 
software and how to use it (Be sure to read this file before using 
the software, because it may contain important information 
about recent changes to the software.)

¥	The subdirectory lib/ with the library file libdf.a, which 
contains the high-level routines described in this manual for 
working with raster image sets and scientific datasets, as well 
as the lower-level general purpose routines for building and 
manipulating HDF routines of any type

¥	The subdirectory bin/ with the executable utility programs

¥	The subdirectory src/, which contains the source code for the 
latest supported version of all programs

¥	The subdirectory include/, which contains the header files 
listed in Appendix B of this manual

¥	The subdirectory examples/, which contains one or more 
sample programs that use HDF

The HDF public directories are currently accessible on the 
CRAY-2, CRAY X-MP, Alliant FX/8 (medusa), Alliant FX/80 
(replicant), and NCSA Sun systems. The pathnames of these 
directories are listed in Table D.1.

Table D.1	Pathnames of NCSA 
HDF Directories
NCSA Computer	Directory Path
CRAY-2	/usr/lib
	CRAY X-MP	/usr/lib
Alliant FX/80 (replicant)	/usr/hdf/lib
Sun systems	/soft/hdf/lib
SGI systems	/rels/shared/soft/hdf


In order to compile a program that uses one of the NCSA HDF 
library routines, you need to link the library to your program when 
you compile or link your program. For example, suppose you have 
a program called myprog.f written in FORTRAN for the CRAY-2 
system. If myprog.f contains calls to HDF routines, you can link 
libdf.a to your program when you compile it by entering:

FORTRAN:
cf77 myprog.f -o myprog -ldf 


where myprog is the name of the executable program.

As another example, suppose you have a program called myprog.c 
written in C for one of the Sun systems. If myprog.c contains 
calls to HDF routines, you can link libdf.a to your program when 
you compile it by entering:

C:
cc  myprog.c -o myprog /soft/hdf/lib/libdf.a 

