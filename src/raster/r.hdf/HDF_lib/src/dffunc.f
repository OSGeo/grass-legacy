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


C	dffunc.f
C
C	Declarations of return values for HDF SDS functions
C
	integer DFSDsetlengths,		dsslens
	integer DFSDsetdims,		dssdims
	integer DFSDsetdimstrs,		dssdist
	integer DFSDdsetdimscale,	dssdisc
	integer DFSDsetdatastrs,	dssdast
	integer DFSDsetmaxmin,		dssmaxm
	integer DFSDputdata,		dspdata
	integer DFSDadddata,		dsadata
	integer DFSDclear,		dsclear
	integer DFSDstartslice,		dssslc
	integer DFSDputslice,		dspslc
	integer DFSDendslice,		dsseslc
	integer DFSDgetdims,		dsgdims
	integer DFSDgetdata,		dsgdata
	integer DFSDgetdimstrs,		dsgdist
	integer DFSDgetdimscale,	dsgdisc
	integer DFSDgetdatastrs,	dsgdast
	integer DFSDgetmaxmin,		dsgmaxm
	integer DFSDreadref,		dsrref
	integer DFSDrestart,		dsfirst
	integer DFSDgetslice,		dsgslc
	integer DFSDsetNT,		dssnt
	integer DFSDgetNT,		dsgnt
	integer DFSDsetorder,		dssodr
	integer DFSDnumber
	integer DFSDlastref,		dslref
        integer dsp32sd,
        integer dsscal
        integer dsgcal
C
C	Declarations of return values for HDF Annotation functions
C
	integer DFANputlabel,		daplab
	integer DFANputdesc,		dapdesc
	integer DFANgetlablen,		dagllen
	integer DFANgetlabel,		daglab
	integer DFANgetdesclen,		dagdlen
	integer DFANgetdesc,		dagdesc
	integer DFANlablist,		dallist
	integer DFANaddfid,		daafid
	integer DFANaddfds,		daafds
	integer DFANgetfidlen,		dagfidl
	integer DFANgetfid,		dagfid
	integer DFANgetfdslen,		dagfdsl
	integer DFANgetfds,		dagfds
	integer DFANlastref
C
C	Declarations of return values for HDF Raster Image functions
C
	integer DFR8setpalette,		d8spal
	integer DFR8putimage,		d8pimg
	integer DFR8addimage,		d8aimg
	integer DFR8getdims,		d8gdims
	integer DFR8getimage,		d8gimg
	integer DFR8readref,		d8rref
	integer DFR8restart,		d8first
	integer DFR8nimages
	integer DFR8lastref

	integer DFR24setil,		d2setil
	integer DFR24addimage,		d2iaimg
	integer DFR24getdims,		d2igimg
	integer DFR24readref,		d2rref
	integer DFR24restart,		d24first
	integer DFR24requil,		d2reqil
        integer d24lref

	integer DFPaddpal,		dpapal
	integer DFPgetpal,		dpgpal
	integer DFPputpal,		dpppal
	integer DFPnpals,		dpnpals
	integer DFPwriteref,		dpwref
	integer DFPreadref,		dprref
	integer DFPrestart,		dprset
	integer DFPlastref,		dplref

C
C	Declarations of return values for HDF Raster Image functions
C
	integer DFopen
	integer DFclose
	integer DFindnextref,		dfindnr
	integer DFsfind
	integer DFfind
	integer DFget
	integer DFput
	integer DFaccess
	integer DFread
	integer DFwrite
	integer DFseek
	integer DFupdate
	integer DFdup
	integer DFdel
	integer DFerrno
	integer DFishdf
	integer DFnewref
	integer DFnumber
	integer DFstat

C End of function declarations




