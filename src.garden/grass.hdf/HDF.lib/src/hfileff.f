c***************************************************************************
c
c
c                         NCSA HDF version 3.2r3
c                            December 1, 1992
c
c NCSA HDF Version 3.2 source code and documentation are in the public
c domain.  Specifically, we give to the public domain all rights for future
c licensing of the source code, all resale rights, and all publishing rights.
c
c We ask, but do not require, that the following message be included in all
c derived works:
c
c Portions developed at the National Center for Supercomputing Applications at
c the University of Illinois at Urbana-Champaign, in collaboration with the
c Information Technology Institute of Singapore.
c
c THE UNIVERSITY OF ILLINOIS GIVES NO WARRANTY, EXPRESSED OR IMPLIED, FOR THE
c SOFTWARE AND/OR DOCUMENTATION PROVIDED, INCLUDING, WITHOUT LIMITATION,
c WARRANTY OF MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE
c
c***************************************************************************
C
C $Header: /hdf/hdf/v3.2r2/src/RCS/hfileff.f,v 1.1 1992/08/25 21:40:44 koziol beta $
C
C $Log: hfileff.f,v $
c Revision 1.1  1992/08/25  21:40:44  koziol
c Initial revision
c
C
C
C------------------------------------------------------------------------------
C File:     hfileFf.f
C Purpose:  Fortran stubs for Palette Fortran routines
C Invokes:  hfileF.c 
C Contents: 
C   hopen:          Call hiopen to open file
C Remarks: none
C----------------------------------------------------------------------------*/


C------------------------------------------------------------------------------
C Name: hopen
C Purpose:  call hiopen, open file
C Inputs:   path: Name of file to be opened
C           access: DFACC_READ, DFACC_WRITE, DFACC_CREATE,
C                      or any bitwise-or of the above.
C           ndds: Number of dds in header block if file needs to be created.
C Returns: 0 on success, FAIL on failure with error set
C Users:    Fortran stub routine
C Invokes: hiopen
C----------------------------------------------------------------------------*/

      integer function hopen(filename, access, defdds)

      character*(*) filename
      integer       access, defdds, hiopen

      hopen = hiopen(filename, access, defdds, len(filename))
      return
      end

