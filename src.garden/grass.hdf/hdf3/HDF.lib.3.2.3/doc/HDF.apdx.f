F.1	NCSA HDF Calling Interfaces and Utilities

NCSA HDF README Files on Anonymous FTP	F.1

National Center for Supercomputing Applications

July 1990

                                                                

July 1990




Appendix  F	NCSA HDF README Files on Anonymous FTP





This appendix includes listings of the README files which can be 
found in the anonymous FTP directory that contains NCSA HDF. 
These listings were made on July 5, 1990 and do not necessarily 
reflect the current contents of the files. The best way to obtain the 
most recent versions is to access them through anonymous FTP. 

Figure F.1. README.FIRST
#*****************************************************************************
#
#                         NCSA HDF version 3.1
#                               July 1, 1990
#
# NCSA HDF Version 3.1 source code and documentation are in the public
# domain.  Specifically, we give to the public domain all rights for future
# licensing of the source code, all resale rights, and all publishing rights.
#
# We ask, but do not require, that the following message be included in all
# derived works:
#
# Portions developed at the National Center for Supercomputing Applications at
# the University of Illinois at Urbana-Champaign.
#
# THE UNIVERSITY OF ILLINOIS GIVES NO WARRANTY, EXPRESSED OR IMPLIED, FOR THE
# SOFTWARE AND/OR DOCUMENTATION PROVIDED, INCLUDING, WITHOUT LIMITATION,
# WARRANTY OF MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE
#
#*****************************************************************************

***********************************************************************

                        NCSA HDF version 3.1
                           July 1, 1990
***********************************************************************

This is NCSA HDF version 3.1.  Suggestions and bug reports are welcome.

Included in this version are:
        the basic low-level routines to perform I/O to HDF files,
        routines to process 8-bit Raster Image Sets
        routines to process Scientific Data Sets.
        routines to process 24-bit Raster Image Sets
        routines to extract slabs from Scientific Data Sets
        routines to process Palettes (independently of images)
        routines to process Annotations for data items

NCSA HDF is the Hierarchical Data Format, a standard file format
Figure F.1	README.FIRST 
(Continued)

developed by NCSA.  For more information about HDF, see the
January/February 1989 NCSA Data Link article, the document "NCSA HDF", and
the document "HDF  Specification".

This version of HDF runs on CRAYs running UNICOS, ALLIANTs, SUNs and
IRIS 4D machines running Unix, MACs running MacOS, VAXen running VMS and
PCs running MS/DOS.

Compilation of these programs produces a library of HDF routines that
can be called from either FORTRAN or C programs.

There is an older version of HDF implemented for CRAYs running CTSS, available
from NCSA.  This version only implements the 8-bit Raster Image Set.  If you
are interested in this, please contact Mike Folk (see below).


This document describes how to obtain a version of HDF for your system.
Other information, including hints on using HDF, descriptions of  the
files that comprise the distribution, and instructions on how to to
create a library, can be found in other README files in this directory.

There are two ways of obtaining HDF, depending on whether you are
accessing this system by remote login or anonymous ftp.
Accordingly, this document contains the following sections:

        Obtaining HDF using anonymous ftp
        Obtaining HDF using remote login

If you have any questions, problems or suggestions, you can contact us
via Email at mfolk@ncsa.uiuc.edu or likkai@ncsa.uiuc.edu,  or by writing
to Mike Folk, Software Development, NCSA, 605 East Springfield Ave.,
Champaign, IL 61820, or call 217 244 0647.


************************************************************************
        Obtaining HDF using anonymous ftp
************************************************************************

Login to ftp.ncsa.uiuc.edu (128.174.20.50), with a login name of
"anonymous". Give your real name as password.  Move to the directory
"HDF" by issuing the command "cd HDF" to ftp.  Now you are ready to
transfer files.  There are two ways to do this:

1. You may use the command "get hdf3.00.tar.Z" to download a compressed
"tar" format file.  (Be sure to set file transfer mode to binary with the
command "binary".)   Unpacking hdf.tar with the Unix "uncompress" and "tar"
utility on your system will produce a tree of subdirectories similar to
the ones in this directory.    These files are described in INSTALL.  They
must be compiled according to the instructions in that section.  (NOTE: this
tar file is very large, as it contains all the source files, plus
all of the documentation.  If space is dear, consider using the method
described in the next paragraph.)

1a) For MacII/MPW users, there is a binhexed stuffit file called
hdf3.00.sit.hqx.  Use ascii mode to get this file, unbinhex it, then
unstuff it.  This will provide you will all the files for the MacII.

2. As an alternative to "tar", you may download the files you require
directly.  Use "cd src" to move to the directory containing source
files.  Then use the command "mget *". If your system is VMS, get the
Figure F.1	README.FIRST 
(Continued)

files in src/fixatr also.  If your system is a MacII and you need to use
fortran, get the fortran files in src/mac.  This will prompt you for each
of the source files, asking if you want to download them. Answer "y" to each.
This will produce the source files for that system in your directory.
Compile these files according to the instructions in the file INSTALL.

To obtain the documentation enter "cd ../../doc" to move to the
directory containing the documentation.  There are two subdirectories,
with "ascii" providing the documentation in readable form, and "word"
providing it in Macintosh Microsoft Word format. Each subdirectory has
two subdirectories, containing the user documentation (NCSA_HDF) and the
technical specification (HDF_Specs), respectively.  The Word files must
be downloaded in binary mode with Macbinary mode enabled.  The ascii
files may be downloaded in ascii mode.


**********************************************************************
                Obtaining HDF using remote login
**********************************************************************

If you have an account on the NCSA Suns, you may download HDF in this way.
To obtain a copy of HDF for a particular system, login to zaphod.ncsa.uiuc.edu,
cd to the directory /sdg/ftp/HDF and use the "transfer" script.

Usage: transfer systemtype hostname [directory]

where systemtype is "unicos", "sun", "alliant", "iris4", "mac",
"vms" or "pc", hostname is the ftp name of the host you want to transfer
the files to, and directory is the directory on the target system in
which you want the files to be placed.

Transfer will create the source files appropriate for the system type,
then open an ftp connection to the target machine and ask you to login.
When you do, it will automatically copy the required files to the target
system in the directory you specified.  It will also deposit all the
documentation, in Macintosh Microsoft Word format if transferring to a
Macintosh, in ascii format otherwise.  It will deposit all the files in
the same directory, as contrasted to using the "tar" approach outlined
in the section on anonymous ftp, which will create a tree of
subdirectories.


*----------------------------------------------------------------------
*  * NCSA HDF Version 3.00 source code and documentation are in the
* public domain.  Specifically, we give to the public domain all rights
* for future licensing of the source code, all resale rights, and all
* publishing rights.
*
* We ask, but do not require, that the following message be included in
* all derived works:
*
* Portions developed at the National Center for Supercomputing
* Applications at the University of Illinois at Urbana-Champaign.
*
*
* THE UNIVERSITY OF ILLINOIS GIVES NO WARRANTY, EXPRESSED OR IMPLIED,
* FOR THE SOFTWARE AND/OR DOCUMENTATION PROVIDED, INCLUDING, WITHOUT
* LIMITATION, WARRANTY OF MERCHANTABILITY AND WARRANTY OF FITNESS FOR
* A PARTICULAR PURPOSE.
*
Figure F.1	README.FIRST 
(Continued)

* If you want to see more software like NCSA HDF, you need to send us a
* letter, email or US mail, telling us what you are doing with NCSA HDF.
* We need to know:
* 1) What science you are working on (an abstract of your work would be
* fine);
* 2) How NCSA HDF has helped you, e.g., whether it has increased your
* productivity or allowed you to perform operations you were unable to
* do before.
*
* We encourage you to cite the use of NCSA HDF , and any other NCSA
* software you have used, in your publications.  A bibliography of your
* work would be extremely helpful.
*
* This is a new kind of shareware.  You share your science and successes
* with us, and we attain the resources necessary to share more software
* like NCSA HDF with you.
*
***********************************************************************


Figure F.2	README.NCSA. for HDF Users of NCSA Computers

#*****************************************************************************
#
#                         NCSA HDF version 3.1
#                               July 1, 1990
#
...
#
#*****************************************************************************

*********************************************************************

                       NCSA HDF version 3.1
                           December 1, 1989
*********************************************************************


*****************************
        Using HDF at NCSA
*****************************

 HDF is installed on various systems at NCSA.  The following is a list
of directories in which HDF is installed on different systems:


       Cray-2:      /usr/local/apps/hdf
       Cray-XMP:    /usr/local/apps/hdf
       Suns:        /soft/hdf
       Medusa:      /usr/hdf
       Replicant:   /usr/hdf

The current version of HDF will be in that directory and its
subdirectories.

In the bin/ directory you will find several HDF utilities, such as:

   hdfls - a utility to display information about the contents of an HDF file.

   hdfrseq - a utility to display a raster 8 bit image remotely using
              the NCSA Interactive Color Raster protocol.
Figure F.2	README.NCSA. for HDF Users 
of NCSA Computers (Continued)

   hdftor8 - a utility to convert an HDF raster 8 bit image into a raw
              raster image.

   r8tohdf - a utility to convert a raw raster image to an HDF raster 8
              bit image.

   hdfcomp - a utility to change the compression scheme used for 8-bit
             raster images in HDF files.

If you execute any of these commands with no parameters, it will display
the list of acceptable parameters.

In the doc/ directory you will find files containing the HDF
documentation.

In the examples/ directory you will find example codes using HDF. The
example codes should be self explanatory.

In the include/ directory you will find the necessary include files for
use in your own programs that link with the HDF library.

In the lib/ directory you will find the HDF library which contains the
necessary subroutines for developing your own HDF applications.

In the src/ directory you will find the source for all of the HDF
utilities and library.

If you have any questions, please contact the NCSA Consulting Office.


Figure F.3	INSTALL:  Instructions for 
Installing HDF
***********************************************************************

                        NCSA HDF version 3.1
                            July 1, 1990
***********************************************************************

The file HINTS contains some hints for errors and unusual cases.

*Compiling and installing
** UNIX
For UNICOS, SGI or fortran compilers that uses only short
names (< 8 characters), see HINTS.

We now provide makefiles for each configuration we support.
These Makefile's are named Mfile.<SystemName>.  In addition,
Mfile.GEN is the generic makefile which you could modify and
use if your configuration is not supported.

Find the Makefile for your system.  If your system is a sun3
or sun4, you could now do
        cp Mfile.SUN Makefile
        make
or just
        make -f Mfile.SUN
to make the libraries.

Make targets available are :
make / make all -- compile and install library and utilities
make allnostub -- compile and install library without

        Fortran stub routines, and utilities
make build -- compile library and utilities
make buildnostub -- compile library (without Fortran stubs)
        and utilities
make libdf.a -- compile library
make libnostub -- compile library with Fortran stub routines
make utils -- compile utilities
make install -- install library and utilities
make clean -- rm intermediate files
make cleanup -- rm all make products

** VMS
Several DCL script files are provided for compilation.
MAKE.COM -- runs MAKELIB.COM and MAKEUTILS.COM
MAKELIB.COM -- makes the full library, DF.OLB
MAKENOF.COM -- makes the library *without* the fortran
                stubs, also DF.OLB
MAKEUTILS.COM -- compiles the utilities

To run MAKE.COM, for example, type @MAKE at the DCL prompt.

Also provided is SETUPUTILS.COM to setup the commands for
the utils to make them easier to use.  Edit this file before
running to customize the directory path.

** MAC
The Macintosh version of HDF is only supported under MPW
3.0, MPW C 3.0, and (if you are using fortran with it) LS
Fortran v2 onwards.

If you have just the generic distribution, the Makefile for
the MAC is in MAKE.HQX, a binhexed version of the MPW
Figure F.3	 INSTALL:  Instructions 
for Installing HDF (Continued)

makefile.  In the Macintosh distribution, the Makefile is
named Makefile.

To generate commands for compiling type
        make
in MPW shell.

Make targets available are :
make / make all -- compile and install library and utilities
make allnostub -- compile and install library without
        Fortran stub routines, and utilities
make build -- compile library and utilites
make buildnostub -- compile library (without Fortran stubs)
        and utilities
make libdf.a -- compile library
make libnostub -- compile library with Fortran stub routines
make utils -- compile utilities
make install -- install library and utilities
make clean -- rm intermediate files
make cleanup -- rm all make products

** PC compatibles
The batch file MAKE.BAT is included to compile the library
and utilities.  Type
        MAKE
in the MS-DOS prompt.

*****

(Note to Macintosh and PC users:  These routines have been compiled and
run successfully on Macs using MPW C Version 2.0.2, and on PCs using
Lattice C  Version 3.0.  We cannot guarantee that they will compile

correctly with other compilers.  We would appreciate any feedback you
can give on experiences you have compiling them on other compilers.

For a non-Unix system, the Makefile may be used as a guide for compiling
the files.  An approximate summary of the procedure is:

  cc -c df.c dfr8.c dfgroup.c dfcomp.c dfimcomp.c dfsd.c dfkit.c
  ar libdf.a df.o dfr8.o dfgroup.o dfcomp.o dfimcomp.o dfsd.o
  ranlib libdf.a

This creates the library file "libdf.a".

To create the utilities "hdfls", "hdfrseq", "r8tohdf", "hdftor8",
"tektohdf", "hdftotek", and "hdfcomp", the procedure is:

     cc hdfls.c libdf.a -o hdfls
     cc hdfrseq.c libdf.a -o hdfrseq
     cc r8tohdf.c libdf.a -o r8tohdf
     cc hdftor8.c libdf.a -o hdftor8
     cc tektohdf.c libdf.a -o tektohdf
     cc hdftotek.c libdf.a -o hdftotek
     cc hdfcomp.c libdf.a -o hdfcomp

To use the program "hdfseq", create "hdfseq" as a symbolic link to the
executable "hdfrseq".  "hdfseq" displays images on the console of a
Sun or Iris workstation.
Figure F.3	INSTALL:  Instructions 
for Installing HDF (Continued)

--------------- Compiling Subsets of HDF ---------------

If you wish to use only some of the HDF Sets, it is possible to create
versions of the library which only contain the desired interfaces.  For
instance, a user who works only with images, but not with raw floating
point data may wish to have only the Raster Image Set (RIS) but not the
Scientific Data Set. The following is the list of source files necessary
for each of the Sets included in the current version of HDF.

Basic Low level HDF:   df.c dfkit.c df.h dfi.h
Basic Low level Fortran: df.c dfF.c dfFf.f dfkit.c df.h dfi.h

8-bit Raster Image Set (RIS-8): dfr8.c df.c dfkit.c dfcomp.c dfimcomp.c
    dfgroup.c df.h dfi.h dfrig.h
8-bit Raster Image Set Fortran: dfr8.c dfr8F.c dfr8Ff.f df.c dfkit.c
    dfcomp.c dfimcomp.c dfgroup.c df.h dfi.h dfrig.h

Scientific Data Set (SDS): dfsd.c df.c dfkit.c dfgroup.c df.h dfi.h dfsd.h
Scientific Data Set Fortran: dfsd.c dfsdF.c dfsdFf.f df.c dfkit.c
    dfgroup.c df.h dfi.h dfsd.h

------------ Compiling C programs with HDF ----------------

To use HDF routines in your program, use "#include dfrig.h", "#include dfsd.h"
etc. at the top of your program, depending on the Sets you are using.
Call the appropriate HDF routines as described in the
documentation. Compile your C program "myprog.c" as follows:

    cc myprog.c libdf.a -o myprog

If the include file "dfrig.h" is in the directory "incdir", and the
library file "libdf.a" is in "libdir", use



Figure F.4  Code Changes:  Changes Made to HDF in Release 3.0 and 3.1

#*****************************************************************************
#
#                         NCSA HDF version 3.1
#                               July 1, 1990
#
...
#*****************************************************************************

These are changes made in release 3.1

*       fixed bug concerning checking the status of opening a file
        with unbuffered i/o

*       Added function DF24readref and DFGRreadref for random access
        of 24-bit rasters

*       Added function DF24restart

*       Added function DF24setil

*       Speed up the DFSDgetdata, DFSDputdata, DFSDadddata,
        DFSDgetslice and DFSDputslice functions, especially for UNICOS
        machines

*       Added functions DFANaddfid, DFABaddfds, DFANgetfidlen,
        DFANgetfid, DFANgetdslen, DFANgetfds, DFANaddfann,
        DFANgetfannlen, DFANgetfann and DFANlastref.

*       Revised DFANlablist so that it returns all ref numbers for a
        given tag

*       Fixed bug with DFSDgetdata where it does not move to the next
        SDG

*       Added some macros to make passing character arrays from
        fortran to C easier

*       Fixed some more minor bugs

*       Recoded some parts for cosmetic reasons

--------------
New features of HDF 3.0 include the following:

Fortran support for Macintosh II, for Language System Fortran and MPW C
3.0.

An interface for basic i/o of 24-bit raster images, which includes the
following routines:

  DF24addimage:appends a 24-bit raster image set to the file.

  DF24getdims: retrieves the dimensions and interlace of the
               image.

  DF24getimage: retrieves the image and stores it in an array.

  DF24reqil:   specifies an interlace to be used in place of the
               interlace indicated in the file when the next raster
               image is read.
An interface for annotating HDF data objects and files, which includes
the following routines:
Figure F.4  Code Changes:  Changes Made to HDF in Release 3.0 and 3.1 (Continued)


  DFANgetlablen: gets length of label of a tag/ref

  DFANgetlabel:  gets label of tag/ref

  DFANgetdesclen: gets length of description of tag/ref

  DFANgetdesc:   gets description of tag/ref

  DFANputlabel:  puts label of tag/ref

  DFANputdesc:   puts description of tag/ref

  DFANlastref:   returns ref of last annotation read or written

  DFANlablist:   gets list of labels for a particular tag


An interface for input and output of 8-bit palettes, including the
following routines:

  DFPaddpal:    appends a palette to a file.

  DFPgetpal:    reads in the next palette in the file.

  DFPputpal:    writes a palette to a file.

  DFPnpals:     indicates number of palettes in a file.

  DFPwriteref:  sets the reference number of the next palette to be
                written.

  DFPreadref:   gets the reference number of the next palette to be
                retrieved.

  DFPrestart:   specifies that the next call to DFPgetpal reads the
                first palette in the file, rather than the next.

  DFPlastref:   returns the value of the reference number most recently
                read or written.



Scientific data set routines for storing and retrieving subsets (slices)
of scientific data, and for choosing optional storage formats and data
types:

  DFSDstartslice: prepares system to write part of a dataset to a file.

  DFSDputslice:   writes part of a dataset to a file.

  DFSDendslice:   indicates write completion for part of a dataset.

  DFSDgetslice:   reads part of a dataset.

  DFSDsettype:    specifies data attributes: data type and
                  representation, system type, and array order.

* new utilities, including the following:

  hdfed:    lets you browse in an HDF file and manipulate some of the
            data
Figure F.4  Code Changes:  Changes Made to HDF in Release 3.0 and 3.1 (Continued)

  fptohdf:  converts floating point data to HDF floating point data
            and/or 8-bit raster images

  r24tohdf: converts a raw RGB 24-bit image to an 8-bit RIS8 with a
            palette

  paltohdf: converts a raw palette to hdf format

  hdftopal: converts palette in an hdf file to raw format
