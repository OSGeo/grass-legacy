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
C $Header: /hdf/hdf/v3.2r2/src/RCS/dfff.f,v 1.1 1992/08/25 21:40:44 koziol beta $
C
C $Log: dfff.f,v $
c Revision 1.1  1992/08/25  21:40:44  koziol
c Initial revision
c
C
C------------------------------------------------------------------------------
C File:     dfFf.f
C Purpose:  Fortran stubs for Fortran low level i/o routines
C Invokes:  dfF.c dfkit.c
C Contents: 
C   dfopen:     call dfiopen to open HDF file
C   dfishdf:    call dfiishdf to find is file is HDF
C -----------------------------------------------------------------------------

C------------------------------------------------------------------------------
C Name:     dfopen
C Purpose:  call dfiopen to open HDF file
C Inputs:   name: name of HDF file to open
C           access: integer for access mode: DFACC_READ etc.
C           defdds: default number of DDs per header block
C Returns:  0 on success, -1 on failure with DFerror set
C Users:    HDF Fortran programmers
C Invokes:  dfiopen
C------------------------------------------------------------------------------


      integer function dfopen(name, access, defdds)
      character*(*) name
      integer access, defdds, dfiopen

      dfopen = dfiopen(name, access, defdds, len(name))
      return
      end

C------------------------------------------------------------------------------
C Name:    dfaccess
C Purpose: call dfiaccess to set up access to a data element
C Inputs:  dfile: pointer to open HDF file
C          tag: tag of element to access
C          ref: ref of element to access
C          access: access mode requested
C Returns: 0 on success, -1 on failure with DFerror set
C Users:   HDF FORTRAN programmers
C Invokes: dfiaccess
C------------------------------------------------------------------------------

      integer function dfaccess(dfile, tag, ref, access)
      character*(*) access
      integer dfile, tag, ref, dfiaccess

      dfaccess = dfiaccess(dfile, tag, ref, access, len(access))
      return
      end

C------------------------------------------------------------------------------
C Name:     dfishdf
C Purpose:  call dfiishdf to test file
C Inputs:   name: name of file to test
C Returns:  0 on success, -1 on failure with DFerror set
C Users:    HDF Fortran programmers
C Invokes:  dfiishdf
C------------------------------------------------------------------------------


      integer function dfishdf(name)
      character*(*) name
      integer dfiishdf

      dfishdf = dfiishdf(name, len(name))
      return
      end
