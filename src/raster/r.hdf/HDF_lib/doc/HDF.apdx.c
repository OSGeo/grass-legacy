C.1	NCSA HDF Calling Interfaces and Utilities

Appendix C	C.1

National Center for Supercomputing Applications

July 1990

                                                                

July 1990




Appendix  C	Eight-Character FORTRAN Names




Some FORTRAN compilers on machines that NCSA supports, such 
as UNICOS FORTRAN (CFT77) on the CRAY-2 system, only 
accept identifier names that are eight or fewer characters. 
Therefore, a set of equivalent names has been devised that can be 
used when programming with one of these compilers. Table C.1 
contains official names, together with the shorter, less descriptive 
versions. Both sets of names are supported on all HDF-supported 
machines.

Table C.1	Long and Short 
Version FORTRAN 
Names
Long Version	Short Version
DF24addimage	d2iaimg
DF24getdims	d2igdims
DF24getimage	d2igimg
DF24reqil(il)	d2reqil
DF24setil	d2setil

DFANgetdesc	dagdesc
DFANgetdesclen	dagdlen
DFANgetlabel	daglab
DFANgetlablen	dagllen
DFANlablist	dallist
DFANlastref	----------
DFANputdesc	dapdesc
DFANputlabel	daplab

DFPaddpal	dpaPal
DFPgetpal	dpgpal
DFPlastref	dplref
DFPnpals	dpnpals
DFPputpal	dpppal
DFPreadref	dprref
DFPrestart	dprest
DFPwriteref	dpwref

DFR8addimage	d8aimg
DFR8getdims	d8gdims
DFR8getimage	d8gimg
DFR8putimage	d8pimg
DFR8restart	d8first
DFR8setpalette	d8spal


Table C.1	Long and Short 
Version FORTRAN 
Names (Continued)
Long Version	Short Version

DFSDadddata	dsadata
DFSDclear	dsclear
DFSDendslice	dseslc
DFSDgetdata	dsgdata
DFSDgetdatalen	dsgdaln
DFSDgetdatastrs	dsgdast
DFSDgetdimlen	dsgdiln
DFSDgetdims	dsgdims
DFSDgetdimscale	dsgdisc
DFSDgetdimstrs	dsgdist
DFSDgetmaxmin	dsgmaxm
DFSDgetslice	dsgslc
DFSDputdata	dspdata
DFSDputslice	dspslc
DFSDrestart	dsfirst
DFSDsetdatastrs	dssdast
DFSDsetdims	dssdims
DFSDsetdimscale	dssdisc
DFSDsetdimstrs	dssdist
DFSDsetlengths	dsslens
DFSDsetmaxmin	dssmaxm
DFSDsettype	dsstype
DFSDstartslice	dssslc


