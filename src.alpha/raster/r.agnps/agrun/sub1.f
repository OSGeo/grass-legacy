CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                          CC
CC                         search                                           CC
CC                                                                          CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      integer*4 function search(rcodiv,ncls)

c This subroutine does a binary search on the numdiv() array to find
c the cell division number that matches "rcodiv".  "rcodiv" is the
c cell number multiplied by 1000 plus the division level.  We will
c find out what the psuedo cell number is (the subscript of "numdiv()")
c and return that value.

	include 'common.blk'

      integer*4 mid,rcodiv,ncls,lo,hi
      save /blk/,/psourc/

c  NCLS         = input, total # of cells including divided cells (I)
c  NUMDIV(ncls) = input, cell # and divisions from nclmap subroutine (I*4)
c  RCODIV       = within, rec. cell # and divisions as computed from 
c                 RCO,RD1,RD2,RD3 (I*4)
c  MID   = middle of "pseudo" cell range presently being searched (I)
c  LO    = low number of "pseudo" cell range presently being searched (I)
c  HI    = high number of "pseudo" cell range presently being searched (I)

      lo = 1
      hi = ncls

c check if out of range to start with

      if (rcodiv.gt.numdiv(hi).or.rcodiv.lt.numdiv(lo)) goto 200

10    mid=int((lo+hi)/2)

c if lo>hi then it could not be found & we return an error flag (-1)

      if(lo.gt.hi) goto 200

c did we find it yet?  if we did then return mid as the result

      if(rcodiv.eq.numdiv(mid)) then
        search=mid
        return
      endif

      if(rcodiv.lt.numdiv(mid)) then
        hi=mid-1
      else
        lo=mid+1
      endif

      goto 10

c if not found or error ...

200   search=-1

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             realnum                                              CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine realnum(numb,nd1,nd2,nd3,numdiv)
      save /blk/,/psourc/
      
c take a combined cell division cell # (cell number*1000+cell division
c components) and return the true cell number and the 3 division level
c numbers.  Used when converting a psuedo cell number to real number 
c because we can get the numdiv number from the psuedo cell number.
c (ie.  NUMDIV(PSUEDO_CELL_#) is passed)

      numb=numdiv/1000
      nd1=(numdiv-numb*1000)/100
      nd2=(numdiv-numb*1000-nd1*100)/10
      nd3=numdiv-numb*1000-nd1*100-nd2*10

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             HYDRO                                                CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  
      subroutine hydro(a,cs,ro,xl,qp,da)

C-Peak flow (cfs) and duration (sec)
C-equation taken from CREAMS model

C- PARAMETERS:

c    A   = input, drainage area (acres)
c    CS  = input, channel slope (% * 10)
c    RO  = input, runoff (in)
c    XL  = input, flow length (ft)
c    QP  = output, peak flow rate (cfs)
c    DA  = output, ave. duration of runoff (seconds)

      integer*4 cs
      real term1,term2
      save /blk/,/psourc/

      if(a.gt.0) then
        term1=ro**(.824*a**0.0166)
        term2=(xl**2./(a*43560.))**(-0.187)
        qp=8.484*a**0.7*(cs/1000.)**0.159*term1*term2
        da=ro*3630.*a/qp
      else
        qp=0.0
        da=0.0
      endif

      end
  

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             CHGE                                                 CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine chge(ss,n,qp,cs,w,t,v)

c--width (ft), shear stress (lbs/ft^2), and velocity (ft/sec)
c--for the channel. equations based on triangular
c--channel and mannings equation

c- parameters:

C    ss = input, % * 10
C    n  = input, Mannings N*1000
C    qp = input, peak flow rate (cfs)
C    cs = input, channel slope (%*10)
C    w  = output, channel width (ft)
C    t  = output, shear stress (lbs/ft**2)
C    v  = output, flow velocity (ft/sec)

C- LOCAL VARIABLES:

C    xcs = channel slope converted to decimal (%*10/1000)
C    xn  = Mannings N*1000 converted to decimal (N*1000/1000)
C    xss = channel side slope converted to decimal (%*10/1000)

      integer*4 ss,cs
      real xss, xcs, xn
      save /blk/,/psourc/

      xss=ss/1000.
      xcs=cs/1000.
      xn=n/1000.
      w=2.05*xss**(-0.625)*(1.+xss**2.)**0.125*(qp*xn/xcs**.5)**0.375
      t=62.4*xss**0.375/(2.*(1.+xss**2.)**.5)**0.75*
     &  xcs**.8125*(xn*qp/1.49)**.375
      v=(1.49/xn)**.75*xss**.25/(2.*(1+xss**2.)**.5)**.5*xcs
     &  **.375*qp**.25

      end

  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             SDFLO                                                CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine sdflo(qp2,chw2,xl,vss,qp1,chw1,qsed1,xkf,ssw,d,
     *                 tw1,v1,tw2,v2,cqsed,qsed2,tc2)

C--SEDIMENT ROUTING BASED ON TRANSPORT CAPACITY

C- PARAMETERS:

C    qp2  = input, peak flow leaving cell (cfs)
C    chw2 = input, channel width leaving cell (ft)
C    xl   = input, flow length across cell (ft)
C    vss  = input, particle settling velocity (ft/sec)
C    qp1  = input, peak flow entering cell (cfs)
C    chw1 = input, channel width entering cell (ft)
C    qsed1= input, sediment flow rate entering cell (lbs/sec)
C    xkf  = input, transport capacity factor
C    ssw  = input, particle specific wght. (lbs/ft**3)
C    d    = input, particle diameter (mm)
C    tw1  = input, shear stress into cell (lbs/ft**2)
C    v1   = input, channel velocity into cell (ft/sec)
C    tw2  = input, shear stress out of cell (lbs/ft**2)
C    v2   = input, channel velocity out of cell (ft/sec)
C    cqsed= input, overland sediment flow rate (lbs/sec)
C    qsed2= output, sediment flow rate leaving cell (lbs/sec)
C    tc2  = output, transport cap. per unit width leaving cell (lbs/sec/ft)

C- LOCAL VARIABLES:

C    eff1  = efficiency term into cell
C    eff2  = efficiency term out of cell
C    tc1   = transport capacity per unit width entering cell (lbs/sec/ft)

      real eff1, eff2, tc1, factor, t1, t2, t3
        
      save /blk/,/psourc/
      eff1=0.
      if(tw1 .gt. 0) eff1=.74*(tw1/((ssw-62.4)*d/304.8))**(-1.98)

      if(tw2.le.0) then
        eff2=0.0
      else
        eff2=.74*(tw2/((ssw-62.4)*d/304.8))**(-1.98)
      endif

      tc1=tw1*v1**2.*xkf/vss*eff1
      tc2=tw2*v2**2.*xkf/vss*eff2
      factor=2.0*qp2/chw2
      t1=factor/(factor+xl*vss)
      t2=0.
      if(chw1 .ne. 0) t2=vss/(qp1/chw1)*(qsed1/chw1-tc1)
      t3=vss*tc2/(qp2/chw2)
      qsed2=t1*(qsed1+cqsed-(chw2+chw1)*xl/4.0*(t2-t3))

      end
  

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             SLBDN                                                CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine slbdn(nstp,k,pct)

C--BREAKDOWN OF THE SOIL INTO PARTICLE SIZES - DEPENDENT ON SOIL TYPE

C-PARAMETERS:

C   NSTP = input, soil type number
C   K    = input, soil particle type number
C   PCT  = output, particle fraction  (%)

C-LOCAL VARIABLES:

C   FDS = matrix of soil type by particle size class (%)

      integer nstp
      real fds(6,5)

      save /blk/,/psourc/
      data fds/.02,.02,.16,.2,.60,1.,.05,.08,.50,.31,.06,1.,
     *         .1,.06,.57,.25,.02,1.,1.,0.,0.,0.,0.,1.,6*0./

      pct=fds(k,nstp)

      END
  

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             RND                                                  CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  
      integer*4 function rnd(x)

C-ROUNDS REAL (X) TO NEAREST INTEGER*4
      
      integer*4 ix
      rnd=x
      ix=rnd
      if(x-ix.ge. 0.5)rnd=rnd+1
      if(x-ix.le.-0.5)rnd=rnd-1
      
      end
  

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             PSNUTR                                               CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  
      subroutine psnutr(i,d,sumn,sump,sumcod)

C This will sum all the nonfeedlot point source nutrients for cell "I".
C If there are no point sources in "I" then return zero values.
C It also fills the PTSN1,PTSP1,PTSC1 arrays with lbs. nutrients.

C PARAMETERS:

C   I      = input, cell number to check for point source nutrients
C   D      = input, duration of channel flow leaving cell "I" (sec)
C   SUMN   = output, sum of N (lbs)
C   SUMP   = output, sum of P (lbs)
C   SUMCOD = output, sum of COD (lbs)

C COMMON VARIABLES:

C   PTSNUM = number of the cell for the IIth point source
C   PTSP   = point source P (ppm)
C   PTSP1  = point source P (lbs)
C   PTSN   = point source N (ppm)
C   PTSN1  = point source N (lbs)
C   PTSC   = point source COD (ppm)
C   PTSC1  = point source COD (lbs)
C   PTSFLW = point source flow
C   LASTPS = number of nonfeedlot point sources in the entire watershed

C CONSTANTS:

C   MAXPS=max # of cells which can have a non-feedlot point source.

      real lbs
      integer*4 ii

	include 'common.blk'

	save /blk/,/psourc/
      sumn=0
      sump=0
      sumcod=0

      do 10 ii=1,lastps

        if(ptsnum(ii).eq.i) then
          lbs= ptsflw(ii)*d*62.4*ptsn(ii)*1e-6
          sumn=lbs + sumn
          ptsn1(ii)=lbs
          lbs= ptsflw(ii)*d*62.4*ptsp(ii)*1e-6
          sump=lbs + sump
          ptsp1(ii)=lbs
          lbs= ptsflw(ii)*d*62.4*ptsc(ii)*1e-6
          sumcod=lbs + sumcod
          ptsc1(ii)=lbs
        endif

10    continue

      end
