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
C $Header: /hdf/hdf/v3.2r2/src/RCS/dfufp2if.f,v 1.1 1992/08/25 21:40:44 koziol beta $
C
C $Log: dfufp2if.f,v $
c Revision 1.1  1992/08/25  21:40:44  koziol
c Initial revision
c
C
C------------------------------------------------------------------------------
C File:     DFUfptoimFf.f
C Purpose:  Fortran stub for DFUfptoimage()
C Invokes:  DFUfptoimage.c
C Contents: 
C   duf2im:       call duif2i_ to invoke DFUfptoimage()
C   dfufptoimage: call duif2i_ to invoke DFUfptoimage()
C -----------------------------------------------------------------------------

C------------------------------------------------------------------------------
C Name:     duf2im
C Purpose:   call duif2i_ to invoke DFUfptoimage()
C Inputs:   
C      hdim, vdim: horizontal and vertical dimensions of input data
C      max, min:   maximum and minimum values in the data
C      hscale,vscale: optional horizontal and vertical scales
C      data:       input data
C      palette:    optional palette to be stored with the image
C      outfile:n   name of hdf file to store image in
C      ct_method:  color transform method: 1=EXPAND; 2=INTERP
C      hres, vres: resolutions desired for output image
C      compress:   compression flag: 0=don't; 1=do
C  Returns: 0 on success, -1 on failure with DFerror set
C  Users:       HDF HLL (high-level library) users, utilities, other routines
C  Invokes: process
C  Remarks: none
C----------------------------------------------------------------------------


      integer function duf2im(hdim,vdim,max,min,hscale,vscale,data,
     *                palette,outfile,ct_method,hres,vres,compress)

      integer       hdim, vdim
      real          max, min, hscale, vscale, data
      character*(*) palette
      character*(*) outfile
      integer       ct_method, hres, vres, compress

      duf2im = duif2i(hdim,vdim,max,min,hscale,vscale,data,palette,
     *              outfile,ct_method,hres,vres,compress, len(outfile))        
      return
      end

CEND7MAX

C------------------------------------------------------------------------------
C Name:     dfufptoimage
C Purpose:   call duif2i_ to invoke DFUfptoimage()
C Inputs:   
C      hdim, vdim: horizontal and vertical dimensions of input data
C      max, min:   maximum and minimum values in the data
C      hscale,vscale: optional horizontal and vertical scales
C      data:       input data
C      palette:    optional palette to be stored with the image
C      outfile:n   name of hdf file to store image in
C      ct_method:  color transform method: 1=EXPAND; 2=INTERP
C      hres, vres: resolutions desired for output image
C      compress:   compression flag: 0=don't; 1=do
C  Returns: 0 on success, -1 on failure with DFerror set
C  Users:       HDF HLL (high-level library) users, utilities, other routines
C  Invokes: process
C  Remarks: none
C----------------------------------------------------------------------------


      integer function dfufptoimage(hdim,vdim,max,min,hscale,vscale,
     *           data, palette,outfile,ct_method,hres,vres,compress)

      integer       hdim, vdim
      real          max, min, hscale, vscale, data
      character*(*) palette
      character*(*) outfile
      integer       ct_method, hres, vres, compress

      dfufptoimage = 
     *             duif2i(hdim,vdim,max,min,hscale,vscale,data,palette,
     *              outfile,ct_method,hres,vres,compress, len(outfile))        
      return
      end

