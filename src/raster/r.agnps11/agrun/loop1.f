cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc                                                                      cc
cc                 LOOP1                                                cc
cc                                                                      cc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine loop1(ei,ge,inp,ncls,nlc,pointfmt,psnum,rain,shpf)

* this subroutine is loop 1 of agnps - It reads in the data from the AGNPS
* input file and does initial calculations for all cells within the watershed

	include 'common.blk'


C------------------------
C  Parameter Description
C------------------------
c The variables have the same description as the main program's dictionary
c of variables and definitions.  The variables designated as "input" below
c are used but not modified and those listed as "output" are modified in 
c this subroutine or subroutines called from this one.

c  EI        input
c  GE        output
c  INP       input
c  NCLS      input
c  NLC       input
c  POINTFMT  output
c  PSNUM     output 
c  RAIN      input
c  see_inp   input
c  SHPF      input

      real ei,ge(6),rain,shpf(3)

      integer*4 inp,ncls,nlc,pointfmt,psnum

      save 
     * /blk/,/psourc/

C------------------------
C  Local variables:     
C------------------------
C  AVLS    :Average land slope in the cell (%)  cp
C  BUFF    :Temporary reading buffer (internal file).
C  C       :Cropping factor  cp
C  CCN     :Cell curve number cp
C  CODF    :COD Factor  cp
C  CS      :Channel slope for the read in (%)  cp
C  EROS    :
C  FERT    :Cell Fertilization level cp
C  FN      :Fertilizer Nitrogen for the cell (lbs/a)
C  FP      :Fertilizer Phosphorous for the cell (lbs/a)
C  FSL     :Field slope length for the cell   cp
C  FRACT   :
C  GULY    :Cell gully erosion (tons)  cp
C  I       :loop index
C  IERR    :Error detection flag
C  IMPF    :Impoundment factor number of ponds in the terrace system  cp
C  INCF    :Cell incorporation of fertilizer (%)  cp
C  IRECV   :Temporary receiving cell variable for loop 1.
C  J       :Loop index
C  NUMB    :Cell number in reading in parameters  cp
C  OD      :Overland flow duration (sec)
C  P       :Practice factor  cp
C  PSID    :Point Source ID number of feedlots  cp
C  RC      :Channel roughness coefficiant for read in  cp
*  RCO     : (see rclmap subroutine)
*  RD1     : "
*  RD2     : "
*  RD3     : "
C  RN      :Runoff N (lbs/a)
C  RND     :Function rounds a real number to integer*4 (HP1000 4 bytes)
C  RO      :Function that calculates runoff
C  RP      :Runoff P (lbs/a)
C  SCC     :Surface Condition Constant for the cell  cp
C  SHAPE   :Cell slope shape code  cp
C  SK      :Soil erodibility factor for each cell  cp
C  SS      :Channel side slope for read in (%)  cp

      integer*4 
     * ccn,codf,fert,fn,fp,fsl,guly,i,ierr,impf,incf,irecv,
     * rnd,j,numb,nd1,nd2,nd3,psid,rco,rd1,rd2,rd3,shape

      real avls,c,cs,eros,fract,od,p,rc,ro,rp,rn,scc,sk,ss

      character*78 buff


c open a scratch file for saving feedlot information for printout later

      open(99,status='unknown')
      
      write(*,'(a\)')'      ... Loop #1 : Reading Input File.         '

      do 110 i=1,ncls

c don't read numb and replace rcel(i) with rco,rd1,rd2,rd3 for cell div.

       read(inp,116) rco,rd1,rd2,rd3,ccn,avls,shape,fsl,cs,
     * ss,rc,sk,c,p,scc,asp(i),sltp(i),fert,incf,psid,guly,codf,impf,
     * chanin(i)

116    format(7x,i4,3i1,i4,f5.1,i2,i4,2f5.1,f5.3,2f4.2,f5.2,f4.2,
     *        3i2,i4,i2,2i4,i3,i2)
       numb=i

c show cell number to screen

      write(*,117) char(13),i
117   format(a,i4)

      if(shape.lt.1.and.sltp(i).eq.0) shape=1
      if(rc.le.0.0.and.sltp(i).eq.0) rc=0.990

c subroutine for assigning rcodiv to rcel(i), this is skipped if no cell
c divisions desired

      call rclmap(ncls,i,numdiv,mxcl,rcel,rco,rd1,rd2,rd3)

      if(fsl.lt.1) fsl=1
      if(cs.lt.0.1) cs=0.1
      if(ss.lt.0.1) ss=0.1
      if(ss.gt.100.) ss=100.
      if(sltp(i).lt.1) sltp(i)=5
      if(rcel(i).eq.i) rcel(i)=0

      irecv=rcel(i)

C MAKE SURE THE CELL OUTLET IS ONE GREATER THAN THE NUMBER OF CELLS

      if (irecv.gt.ncls) then
        irecv=ncls+1
        rcel(i)=irecv
C- set the cell outlet number
        nlc=i
      endif

C PCELL WILL CONTAIN THE NUMBER OF CELLS WHICH DRAIN INTO EACH GIVEN CELL.
C AFTER LOOP 1 IF PCELL(I)=0 THEN I IS A PRIMARY CELL
  
      if(irecv.gt.0) pcell(irecv)=pcell(irecv)+1
      cchs(i)=int(cs*10.)
      chss(i)=int(ss*10.)
      coef(i)=int(rc*1000.)
      call xeros(avls,fsl,ei,sk,c,p,shpf(shape),eros)
      cro(i)=ro(rain,ccn)
      call ovrld(avls,fsl,scc,od)
      cd(i)=od
      ttllbs(i)=eros*2000.*area(i)

      do 125 j=1,5
        call slbdn((sltp(i)),j,fract)
        sme(i,j)=guly*fract*2000.
        ge(j)=guly*fract+ge(j)
125   continue

      ge(6)=ge(6)+guly

      call slnut(fert,incf,rain,(sltp(i)),(cro(i)),rn,rp)

      cron(i)=rnd(rn*100)
      crop(i)=rnd(rp*100)
      ccod(i)=codf*cro(i)*(3630.*62.4)/10.**6.
      soln(i)=rn*area(i)
      solp(i)=rp*area(i)
      scod(i)=codf*cro(i)*(3630.*62.4)/10.**6.*area(i)

      if(irecv .eq. 0) then
        soln(i)=0.0
        solp(i)=0.0
        scod(i)=0.0
      endif

      call do_impound(impf,inp,buff,i,irecv,eros)
      call do_pntsrc(psid,buff,inp,pointfmt,rain,irecv,psnum,i)
  
110   continue
  
      lastps=psnum-1
      close(inp)

c write a zero for termination of scratch file info. & rewind it for
c reading later.

      write(99,*) '0'
      rewind 99

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc                                                                      cc
cc                 do_pntsrc                                            cc
cc                                                                      cc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine do_pntsrc(psid,buff,inp,pointfmt,
     *                     rain,irecv,psnum,i)

c-------------------
c  parameters:
c-------------------

      integer*4     psid,inp,pointfmt,irecv,psnum,i
      character*(*) buff
      real          rain

      save
     * /blk/,/psourc/
c-------------------
c local variables
c-------------------

      integer*4 ln,feedlt,j,rnd,psflow,pscode
      real      psn,psp,pscod,fdlotn,fdlotp,fdcod

c fdcod : COD yield in lbs. for all feedlots in a cell
c fdlotn: N yield in lbs. for all feedlots in a cell
c fdlotp: P yield in lbs. for all feedlots in a cell
c feedlt: feedlot flag to tell if there are 1 or more feedlots in cell
c j     : loop variable (point source number)
c ln    : number of the feedlot in the cell
c pscod : point source (and feedlot) COD (lbs)
c pscode: tells if the point source is a feedlot or not
c psflow: point source flow (cfs)
c psn   : point source nitrogen (lbs)
c psp   : point source phosphorous (lbs)
c rnd   : function to round a real number to an integer

	include 'common.blk'


C ***** IS THERE A POINT SOURCE IN THIS CELL? (if not then return) *****

      if(psid.eq.0) return

C initialize the feedlot number used in FDLOT subroutine

      ln=0

C initialize total FEEDLT point source N,P & COD

      fdlotn=0.
      fdlotp=0.
      fdcod=0.

C initialize FEEDLT flag to tell if there was a feedlot in cell I

      feedlt=0
  
C do from 1 to number of point sources in the cell
  
      do 175 j=1,psid

        call set_pntsrc(pointfmt,inp,buff)
  
C read in 1st line of point source (or re-read if we just determined the
C point source format).

        read(inp,'(a)') buff
  
C if old format then set PSCODE=2 for feedlots

        if(pointfmt.eq.1) then
          pscode=2
        else
          read(buff,'(i1)') pscode
        endif

C-- stream point source
  
       if(pscode.eq.1) then
         read(buff,172) psflow,psn,psp,pscod
172      format(1x,i5,3f7.1)
         uf(i)=uf(i)+psflow

C save which cell this point source data is for

         ptsnum(psnum)=i

c save all flow & nutrient nonfeedlot point source data for this point src.

         ptsflw(psnum)=psflow
         ptsn(psnum)=psn
         ptsp(psnum)=psp
         ptsc(psnum)=pscod
         psnum=psnum+1
 
C-- feedlot point source
  
       elseif (pscode.eq.2) then
         call fdlot(i,ln,rain,psn,psp,pscod,inp,buff,pointfmt)
         feedlt=feedlt+1
         cron(i)=rnd(cron(i)+psn*100./area(i))
         crop(i)=rnd(crop(i)+psp*100./area(i))
         ccod(i)=ccod(i)+pscod/area(i)
         fdlotn=fdlotn+psn
         fdlotp=fdlotp+psp
         fdcod=fdcod+pscod

         if(irecv .gt. 0) then
           soln(i)=soln(i)+psn
           solp(i)=solp(i)+psp
           scod(i)=scod(i)+pscod
         endif
       endif
  
175   continue
  
C if we want a printout of point source data then do the following

      if(feedlt.gt.1) then

c write a "2" out which signifies that this is a total line following

        write(99,*) '2'

c save feedlot totals for cell

        write(99,*) fdlotn,fdlotp,fdcod
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc                                                                      cc
cc                 set_pntsrc                                           cc
cc                                                                      cc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine set_pntsrc(pointfmt,inp,buff)

c------------------
c  parameters:
c------------------

      integer*4     inp, pointfmt
      character*(*) buff

      save /blk/,/psourc/
c------------------
c  local variables 
c------------------

      integer*4 pscode

c pscode: tells if the point source is a feedlot or not

c if POINTFMT=0 then we don't know point source format yet
c if POINTFMT=1 then point source format is the old way
c if POINTFMT=2 then point source format is the new way

c if the point source is not known yet then try to figure it out

        if(pointfmt.eq.0) then
          read(inp,'(a)',end=9999) buff

C backup 1 line so the read will work after the format is determined

          backspace inp
          read(buff,'(i1)') pscode

C if 1st digit is a blank (reads as zero) or not a valid code (ie.
c not a 1 or 2) OR if characters 11 through 13 are blank then we
c assume it is in the old format

          if(pscode.gt.2 .or. pscode.lt.1 .or. buff(11:13).eq.'   ')then
            pointfmt=1
c'Assuming old feedlot format!'
          else
            pointfmt=2
c'Assuming new point source format!'
          endif
        endif

9999  end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc                                                                      cc
cc                 do_impound                                           cc
cc                                                                      cc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine do_impound(impf,inp,buff,i,irecv,eros)

c------------------
c  parameters:
c------------------

      integer*4     impf,inp,i,irecv
      real          eros
      character*(*) buff

	include 'common.blk'

	save /blk/,/psourc/
c------------------
c local variables
c------------------

      integer*4    ierr,j,pd,L
      real         pa,pqp,ptns
      character*14 ifmt

c ierr  : i/o error trapping variable
c ifmt  : variable format for the reading of impoundments
c j     : loop variable (impoundment number)
c l     : loop variable
c pa    : pond drainage area (acres)
c pd    : pipe diameter for the pond in the terrace system (in)
c pqp   : peak flow leaving pond (cfs)
c ptns  : sediment leaving pond for the particle size (tons)


C if there is an impoundment then ...

      if(impf.gt.0) then

        read(inp,'(a)') buff
        backspace inp

        do 140 j=1,impf

        if (j.eq.1) then
          read(inp,'(a)') buff
          read(buff,150) pa,pd
150       format(f4.1,i2)
        else
          write(ifmt,160) j*6-6
160       format('('i2'x,f4.1,i2)')
          read(buff,ifmt) pa,pd
        endif

c if this cell does not drain out (ie. sink hole) then we
c do not want the impoundment calculations to be performed.

          if(irecv.ne.0) then
            if(pa .eq. 0) pa=2.
            if(pd .eq. 0) pd=3
            call terh((cro(i)),pa,pd,pqp)
            df(i)=df(i)+pqp
            do 165 l=1,5
              call ters(sltp(i),l,eros,cro(i),pa,pd,ptns)
              sedy(i,l)=sedy(i,l)+ptns
165         continue
            ant(i)=ant(i)-pa*10.
          endif
  
140     continue
        if(ant(i).lt.0) ant(i)=0.
      endif

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             RO                                                   CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  
      function ro(p,cn)

C-RUNOFF BY SCS CURVE NUMBER METHOD (in)
C- Parameters:

c   P  = input of precip. (in)
c   CN = input of curve number

c- Local variables:

c   FACTOR = temporary variable
c   TOP    =   "         "

      real factor,top
      integer*4 cn

      ro=0.0001
      factor=1000./cn-10.
      top=p-.2*factor
      if(top .le. 0.0) goto 10
      ro=top**2./(p+0.8*factor)

10    end
  

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             XEROS                                                CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine xeros(s,l,xr,xk,xc,xp,ssf,e)

C--USLE EROSION EQUATION
C- PARAMETERS:

c    S   = input, land slope (%)
c    L   = input, field slope lenght (ft)
c    XR  = input, storm erosivity (EI)
c    XK  = input, soil erodibility factor
c    XC  = input, cropping mgmt. factor
c    XP  = input, conservation practice factor
c    SSF = input, slope shape factor
c    E   = output, upland erosion (tons/acre)

C- LOCAL VARIABLES:

c    XL  = slope length factor in USLE
c    XM  = slope length exponent in USLE
c    XS  = slope steepness factor in USLE

      real xm,xl,xs

      save /blk/,/psourc/
      xm=.40
      if (s .lt. 4) xm=.30
      if (s .ge. 5) xm=.50
      xl=(l/72.6)**xm
      xs=(430.*(s/100.)**2.+30.*(s/100.)+.43)/6.574
      e=xr*xk*xl*xs*xc*xp*ssf

      end
  

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             OVRLD                                                CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine ovrld(s,l,oscc,oft)

C--OVERLAND FLOW DURATION (SEC)
C- PARAMETERS:

c    S     = input/output, land slope (%)
c    L     = input, slope length (ft)
c    OSCC  = input, surface condition constant
c    OFT   = output, overland flow duration (sec)

C- LOCAL VARIABLES:

c    V = flow velocity (ft/sec)

      real pwr,v
      save /blk/,/psourc/
      
      if(s .le. 0) s=.01
      pwr=.5*alog10(s)-oscc
      v=10.**pwr
      if(oscc .eq. -.18) goto 5
      if(v .ge. 2.) v=2.0
5     oft=l/v

      end
  

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             SLNUT                                                CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine slnut(nfrt,if,p,nstp,ro,ron,rop)

C--SUBROUTINE TO CALCULATE SOLUBLE NUTRIENT YIELDS
C--TAKEN OUT OF THE EROSION MODEL CREAMS

C- Parameters:

c    NFRT = input, fertilization level
c    EFN   = fert. N (lbs/acre) 
c    EFP   = fert. P (lbs/acre) 
c    IF   = input, incorp. factor (%)
c    P    = input, rainfall (in)
c    NSTP = input, soil type number
c    RO   = input, runoff (in)
c    RON  = output, runoff N (lbs/acre)
c    ROP  = output, runoff P (lbs/acre)

C- Local variables:

c    BD    = bulk density of this soil type number (g/cc)
c    BLKD  = bulk density of major soil textures (g/cc)
c              1) 1.6 = sand
c              2) 1.3 = silt
c              3) 1.4 = clay
c              4) 1.2 = peat?
c              5) 0.0 = water
c    CHECKN= available N due to rainfall (g/g)
c    CHECKP= available P due to initial P in soil surface ( ? ) 
c    CN    = N concentration in soil (ppm) (5)
c    COEFF = porosity factor (cc/cc)
c    COLP  = soluble P in soil surface (kg/h) 
c    CP    = P concentration in soil (ppm) (2)
c    CZERON= available soluble N in soil surface & fert. (kg/h) 
c    CZEROP= available soluble P in soil surface & fert. (kg/h) 
c    EFI   = effective infiltration (mm)
c    EFRAIN= effective rainfall after initial abstraction (mm)
c    EXKN1 = extraction coefficient for movement of N into soil (.25) 
c    EXKN2 = extraction coefficient for movement of N into runoff (.05) 
c    EXKP1 = extraction coefficient for movement of P into soil (.25) 
c    EXKP2 = extraction coefficient for movement of P into runoff (.025)
c    FA    = amount of fert. remaining on surface ( ? ) 
c    FN    = fert. N (kg/h) 
c    FP    = fert. P (kg/h) 
c    POR   = porosity (cc/cc) 
c    RCN   = N concentration in rainfall (ppm) (.8) 
c    RNFALL= rain fall (mm) 
c    RUNOFF= runoff (mm)
c    SOLN  = soluble N in soil surface & fert. (kg/h) 
c    SOLP  = soluble P in soil surface & fert. (kg/h) 
c    TOTPOR= 10*porosity (10*cc/cc) 
c    XKFN1 = rate constant for downward movement of N into soil 
c    XKFN2 = rate constant for movement of soluble N into runoff
c    XKFP1 = rate constant for downward movement of P into soil 
c    XKFP2 = rate constant for movement of soluble P into runoff
  
      integer nstp
      integer*4 efn,efp
      real blkd(5),cn,cp,rcn,exkn2,exkp2,exkn1,exkp1,bd,fa,rnfall,
     & runoff,por,fn,fp,soln,solp,colp,totpor,efrain,efi,coeff,czeron,
     & czerop,checkn,checkp,xkfn1,xkfn2,xkfp1,xkfp2

      data blkd/1.6,1.3,1.4,1.2,0./
      data cn,cp,rcn,exkn2,exkp2,exkn1,exkp1/5.,2.,.8,.05,.025,.25,.25/
      save /blk/,/psourc/

      bd=blkd(nstp)
      fa=if/100.

      if (nfrt.eq.4) then
        read(2,69) efn,efp
      else
        call fertl(nfrt,efn,efp)
      endif

69    format(I4,I4)

      rnfall=p*25.4
      runoff=ro*25.4
      por=1-(bd/2.65)
      fn=efn*1.1206
      fp=efp*1.1206
      soln=0.10*cn*por
      solp=0.10*cp*por
      colp=solp
      ron=rcn*rnfall*.01
      soln=soln+(fn*fa)
      solp=solp+(fp*fa)
      totpor=10.0*por
      efrain=rnfall-totpor
      if(efrain .lt. 0.0) efrain=0.0
      efi=efrain-runoff

      if(nstp.gt.4) goto 2
      coeff=0.00001/por
      czeron=soln*coeff
      czerop=solp*coeff
      checkn=rcn*1.0e-06
      checkp=colp*coeff
      xkfn1=exkn1/totpor
      xkfn2=exkn2/totpor
      xkfp1=exkp1/totpor
      xkfp2=exkp2/totpor
      if(efrain .le. 0.0) goto 5

      ron=((czeron-checkn)*exp(-xkfn1*efi)-(czeron-checkn)*
     *  exp(-xkfn1*efi-xkfn2*runoff))/coeff + ron*runoff/efrain
      rop=((czerop-checkp)*exp(-xkfp1*efi)-(czerop-checkp)*
     *  exp(-xkfp1*efi-xkfp2*runoff))/coeff+checkp*xkfp2*runoff/coeff

2     ron=ron*2.205/2.471
      rop=rop*2.205/2.471
      goto 10

5     ron=0.0
      rop=0.0

10    continue

      end
  

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             FERTL                                                CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine fertl(nfrt,efn,efp)

C--FERTILIZER ESTIMATES BASED ON HIGH,AVERAGE, AND LOW applications

C - PARAMETERS:

c    NFRT - input, fert. level
c    EFN  - output, fert. N (lbs/acre)
c    EFP  - output, fert. P (lbs/acre)

C - LOCAL VARIABLES:

c    NPPA - N fert. application rate (lbs/acre)
c    PPPA - P fert. application rate (lbs/acre)

      integer*4 efn,efp,pppa(4),nppa(4),nf

      data nppa/0,50,100,200/
      data pppa/0,20,40,80/

      save /blk/,/psourc/

      nf=nfrt+1
      efn=nppa(nf)
      efp=pppa(nf)

      end
  

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             FDLOT                                                CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  
      subroutine fdlot(cell,ln,rain,psn,psp,pso,inp,buff,fmt)

C--Calculates nutrient yields from feedlots taken from MPCA feedlot model
C--developed by R. Young, USDA, Morris, MN.
  
C PARAMETERS:

c   CELL   - input, cell # of this feedlot
c   BUFF   - input, Line of feedlot data already read in main program.
c   FMT    - input, format of 1st line: 1=old way, 2=new way (skip 1 char)
c   INP    - INPUT FILE UNIT NUMBER
c   LN     - LOT NUMBER
c   see_feed-input, true=print feedlot data, false=do not
c   NP     - PRINTOUT NUMBER
c   PSN    - POINT SOURCE N YIELD IN LBS DUE TO THE FEEDLOT
c   PSO    - POINT SOURCE COD YIELD IN LBS DUE TO THE FEEDLOT
c   PSP    - POINT SOURCE P YIELD IN LBS DUE TO THE FEEDLOT
c   RAIN   - PRECIPITATION IN INCHES

C LOCAL VARIABLES:

c   A1     - FEEDLOT AREA IN ACRES
c   A2     - AREA 2 (TRIBUTARY) SUBAREAS IN ACRES
c   A3     - AREA 3 (ADJACENT) SUBAREAS IN ACRES
c   ALOG10 - real function, log. base 10
c   ARF    - ROOF AREA IN ACRES
c   BC     - BUFFER AREA SURFACE CONDITION CONSTANT
c   BCCL   - BACKGROUND CONCENTRATION OF N IN PPM - SET TO 12 AT BEGINNING
c            OF SUBROUTINE
c   BLNG   - LENGTH OF BUFFER AREA IN FEET
c   BSLP   - SLOPE OF BUFFER AREA IN %
c   CN1    - CURVE NUMBER FOR AREA 1 (FEEDLOT)
c   CN2    - CURVE NUMBER FOR AREA 2 (TRIBUTARY)
c   CN3    - CURVE NUMBER FOR AREA 3 (ADJACENT)
c   DGWN   - % DECREASE OF N DUE TO GRASS WATERWAY
c   DGWO   - % DECREASE OF COD DUE TO GRASS WATERWAY
c   DGWP   - % DECREASE OF P DUE TO GRASS WATERWAY
c   DOFN   - % DECREASE OF N DUE TO OVERLAND FLOW
c   DOFO   - % DECREASE OF COD DUE TO OVERLAND FLOW
c   DOFP   - % DECREASE OF P DUE TO OVERLAND FLOW
c   DPN    - AMOUNT OF N IN RUNOFF AT FEEDLOT EDGE IN LBS
c   DPNC   - N CONCENTRATION AT DISCHARGE POINT IN PPM
c   DPNM   - AMOUNT OF N IN RUNOFF AT DISCHARGE POINT IN LBS
c   DPO    - AMOUNT OF COD IN RUNOFF AT FEEDLOT EDGE IN LBS
c   DPOC   - COD CONCENTRATION AT DISCHARGE POINT IN PPM
c   DPOM   - AMOUNT OF COD IN RUNOFF AT DISCHARGE POINT IN LBS
c   DPP    - AMOUNT OF P IN RUNOFF AT FEEDLOT EDGE IN LBS
c   DPPC   - P CONCENTRATION AT DISCHARGE POINT IN PPM
c   DPPM   - AMOUNT OF P IN RUNOFF AT DISCHARGE POINT IN LBS
c   EAUN   - EQUIVALENT ANIMAL UNITS OF N
c   EAUO   - EQUIVALENT ANIMAL UNITS OF COD
c   EAUP   - EQUIVALENT ANIMAL UNITS OF P
c   F1     - 1ST FEEDLOT RATING FACTOR, COMPRESSES RANGE TO LOG SCALE (0-1)
c   F2     - 2ND FEEDLOT RATING FACTOR, WEIGHTS F1
c   FLN    - N CONCENTRATION OF RUNOFF AT FEEDLOT EDGE IN PPM
c   FLO    - COD CONCENTRATION OF RUNOFF AT FEEDLOT EDGE IN PPM
c   FLP    - P CONCENTRATION OF RUNOFF AT FEEDLOT EDGE IN PPM
c   FN     - N FACTOR OF FEEDLOT ANIMAL
c   FO     - COD FACTOR OF FEEDLOT ANIMAL
c   FP     - P FACTOR OF FEEDLOT ANIMAL
c   GWTC   - GRASS WATERWAY TIME OF CONTACT FACTOR FOR DECREASE IN POLLUTANT
c            STRENGTH
c   J      - loop index
c   NA     - NUMBER OF ANIMALS
c   NFRATE - FEEDLOT RATING NUMBER (INTEGER)
c   OFTC   - OVERLAND FLOW TIME OF CONTACT FACTOR FOR DECREASE IN POLLUTANT
c            STRENGTH
c   RDN    - PERCENT OF N STRENGTH REMAINING
c   RDO    - PERCENT OF COD STRENGTH REMAINING
c   RDP    - PERCENT OF P STRENGTH REMAINING
c   RNOF   - RUNOFF IN INCHES (CALCULATED FROM RAIN AND CN)
c   RO     - RUNOFF IN INCHES
c   TC     - TIME OF CONTACT IN SEC
c   VOL1   - VOLUME OF RUNOFF FROM AREA 1 IN ACRE-INCHES
c   VOL1T  - TRANSFORMED VOL1 (VOLUME OF RUNOFF FROM FEEDLOT AND AREA 2 WHOSE
c           CONCENTRATION IS THE SAME AS UNDILUTED FEEDLOT RUNOFF
c   VOL2   - VOLUME OF RUNOFF FROM AREA 2 IN ACRE-INCHES
c   VOL2T  - TRANSFORMED VOL2 (VOLUME OF RUNOFF FROM AREA 2 THAT DOES NOT
c            CONTRIBUTE POLLUTANTS BUT DILUTES)
c   VOL3   - VOLUME OF RUNOFF FROM AREA 3 IN ACRE-INCHES
c   VOLTT  - TOTAL VOLUME OF RUNOFF IN ACRE-INCHES AT THE DISCHARGE POINT
c   XMPN   - % MANURE PACK OF N
c   XMPO   - % MANURE PACK OF COD
c   XMPP   - % MANURE PACK OF P

      integer*4 blng(3),cn1,cn2(6),cn3(6),cell,fmt,na(3),nfrate
      integer*4 j
      character*78 buff
      real a2(6),a3(6),bslp(3),bc(3),fo(3),fp(3),fn(3),bccl,a1,arf,
     & vol2,vol3,oftc,gwtc,dgwo,dgwp,dgwn,eauo,eaup,eaun,rnof,ro,vol1,
     & tc,xmpo,xmpp,xmpn,dofo,dofp,dofn,rdo,rdp,rdn,vol1t,vol2t,voltt,
     & flo,flp,fln,dpo,dpp,dpn,dpoc,dppc,dpnc,dpom,dppm,dpnm,codmas,f1,
     & f2

      data bccl/12./

      save /blk/,/psourc/

C increment the lot number(LN) - counts the lot number in each cell, set
C to zero in loop 1 when calculations for that cell began.

      ln=ln+1
      psn=0
      psp=0
      pso=0

      if(fmt.eq.2) then
        read(buff,5) a1,cn1
5       format(1x,f6.2,i4)
      else
        read(buff,6) a1,cn1
6       format(f6.2,i4)
      endif

      read(inp,10) arf,(a2(j),j=1,6),(cn2(j),j=1,6)
10    format(7f6.2,6i4)

      read(inp,15) (a3(j),j=1,6),(cn3(j),j=1,6)
15    format(6f6.2,6i4)

      read(inp,20) bslp(1),bc(1),blng(1),bslp(2),bc(2),
     *blng(2),bslp(3),bc(3),blng(3)
20    format(f5.1,f5.2,i4,f5.1,f5.2,i4,f5.1,f5.2,i4)

      read(inp,25) na(1),fo(1),fp(1),fn(1),na(2),fo(2),fp(2),
     *fn(2),na(3),fo(3),fp(3),fn(3)
25    format(i5,3f5.2,i5,3f5.2,i5,3f5.2)

      vol2=0.
      vol3=0.
      oftc=0.01
      gwtc=0.
      dgwo=0.
      dgwp=0.
      dgwn=0.
      eauo=0.
      eaup=0.
      eaun=0.

      rnof=ro(rain,cn1)
      vol1=rnof*a1

      do 100 j=1,6
        if(cn2(j) .eq. 0) goto 105
        rnof=ro(rain,cn2(j))
100   vol2=vol2+rnof*a2(j)

105   vol2=vol2+rain*arf

      do 110 j=1,6
        if(cn3(j) .eq. 0) goto 115
        rnof=ro(rain,cn3(j))
110     vol3=vol3+rnof*a3(j)

115   do 120 j=1,3
        if(bc(j) .eq. 0) goto 125
        if(bc(j) .eq. 1.0) bc(j)=-.18
        call ovrld(bslp(j),blng(j),bc(j),tc)
        if(bc(j) .eq. -.18) goto 130
        oftc=oftc+tc
        goto 120
130     gwtc=gwtc+tc
120   continue

125   do 200 j=1,3
        if(na(j) .eq. 0) goto 205
        eauo=eauo+na(j)*fo(j)
        eaup=eaup+na(j)*fp(j)
200   eaun=eaun+na(j)*fn(j)

205   xmpo=eauo/a1
      xmpp=eaup/a1
      xmpn=eaun/a1
      if(xmpo .ge. 100) xmpo=100.
      if(xmpp .ge. 100) xmpp=100.
      if(xmpn .ge. 100) xmpn=100.
      dofo=-27.9+42.8*alog10(oftc)
      dofp=-49.3+50.5*alog10(oftc)
      dofn=-16.8+42.3*alog10(oftc)

      if(gwtc.gt.0) then
        dgwo=15.95+.033*gwtc
        dgwp=-21.2+.036*gwtc
        dgwn=25.5+.047*gwtc
      endif

      if(dofo .gt. 100) dofo=100.
      if(dofo .lt. 0) dofo=0.
      if(dofp .gt. 100) dofp=100.
      if(dofp .lt. 0) dofp=0.
      if(dofn .gt. 100) dofn=100.
      if(dofn .lt. 0) dofn=0.
      if(dgwo .gt. 100) dgwo=100.
      if(dgwo .lt. 0) dgwo=0.
      if(dgwp .gt. 100) dgwp=100.
      if(dgwp .lt. 0) dgwp=0.
      if(dgwn .gt. 100) dgwn=100.
      if(dgwn .lt. 0) dgwn=0.
      rdo=(1-dgwo/100.)*(1-dofo/100.)
      rdp=(1-dgwp/100.)*(1-dofp/100.)
      rdn=(1-dgwn/100.)*(1-dofn/100.)
      if(vol2 .gt. 30) goto 300
      vol1t=vol1+vol2
      vol2t=0.
      goto 305

300   vol1t=vol1+30.
      vol2t=vol2-30.

305   voltt=vol1t+vol2t+vol3
      flo=(vol1t*xmpo*45.)+(vol2t*60.)
      flp=(vol1t*xmpp*.85)+(vol2t*2.)
      fln=(vol1t*xmpn*3.)+(vol2t*bccl)
      dpo=flo*rdo+vol3*60.
      dpp=flp*rdp+vol3*2.
      dpn=fln*rdn+vol3*bccl
      dpoc=dpo/voltt
      dppc=dpp/voltt
      dpnc=dpn/voltt
      dpom=dpo*(3630.*62.4)/10.**6.
      dppm=dpp*(3630.*62.4)/10.**6.
      dpnm=dpn*(3630.*62.4)/10.**6.

C calculating feedlot rating number

      codmas=xmpo*45.0*rdo*vol1t*.2265

      if(codmas.le.0) then
        nfrate=0
      else
        f1=(log10(codmas)-2.0)/3.0
        f2=.8+(.1*log10(voltt))
        nfrate=int(f1*f2*100.)
      endif

C fixed it so that a negative feedlot rating is set to zero.

      if(nfrate.lt.0) nfrate=0
  
      psn=dpnm
      pso=dpom
      psp=dppm

c write out code and values to scratch file for later printing

      write(99,*) '1'
      write(99,*) ln,cell,dpnc,dppc,dpoc,dpnm,dppm,dpom,nfrate

      end
  

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             TERH                                                 CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  
      subroutine terh(ro,a,d,qp)

C--SUBROUTINE TO CALCULATE THE PEAK FLOW FROM AN IMPOUNDMENT
C--RELATIONS TAKEN FROM CREAMS POND MODEL

C PARAMETERS:

c  RO - input, runoff from impoundment (in)
c  A  - input, drainage area of impoundment (acres)
c  D  - input, diameter of the pipe outlet (in)
c  QP - output, peak outflow from impoundment area (cfs)

C LOCAL VARIABLES:

c  TVOL - total volume of runoff (ft**3)
c  FS   - ??
c  B    - ??
c  Y    - ??

      integer*4 d
      real cor,y,tvol,b,fs

      data fs,b/7500.,1.5/

      save /blk/,/psourc/

      tvol=ro*3630.*a
      y=(3.*tvol/fs)**(1/(1+b))
      cor=13968.*(d/12.)**2.
      qp=cor*y**.5/3600.

      end
  

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             TERS                                                 CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  
      subroutine ters(nstp,l,tpa,ro,a,d,psd)

C--SUBROUTINE TO CALCULATE SEDIMENT YIELD FROM AN IMPOUNDMENT
C--RELATIONS TAKEN FROM CREAMS POND ELIMENT
C--RELATIONS CALCULATE THE YIELD IN LBS FROM THE FRACTION
C--REPRESENTING THE PORTION OF THE PARTICULAR PARTICLE SIZE
C--PASSING THROUGH THE TERRACE

C PARAMETERS:

c   NSTP - input, soil type number
c   L    - input, particle type number
c   TPA  - input, sediment entering the pond (tons/acre)
c   RO   - input, runoff (in)
c   A    - input, drainage area of pond (acres)
c   D    - input, pipe diameter of outlet (in)
c   PSD  - output, sediment leaving pond (tons/acre)

C LOCAL VARIABLES:

c   TINF - infiltration rate for major textural classes (in/hr)
c   FS,B - coefficients in eqn. relating pond area to depth
c   TVOL - total runoff volume (ft**3)
c   FRACT- particle fraction (%)
c   FP   - fraction of a particle class passing thru impoundment (%)
c   EQSD - equivalent sand diameter for a particle class (microns)
c   D1   - equivalent sand diameter for a class L (microns)
c   DU   - equivalent sand diameter for next larger class (microns)
c   DC   - difference between DU and D1 (microns)

      integer nstp
      integer*4 d,eqsd(6)
      real tinf(4),fract,d1,dc,fp,du,b1,a1,ys,zs,cor,tvol,b,fs

      data eqsd/0,2,10,20,158,201/
      data tinf/.70,.40,.05,1.5/
      data fs,b/7500.,1.5/
  
      save /blk/,/psourc/
      if(nstp.gt.4) return
  
      tvol=ro*3630.*a
      cor=13968.*(d/12.)**2.
      zs=-6.68e-06*fs-.0903*b+1.19e-04*cor-3.42e-06*tvol
     *   -20400.*tinf(nstp)/43200.
      ys=3.28e-05*fs+.123*b-2.4e-04*cor+8.10e-06*tvol
     *   -11880.*tinf(nstp)/43200.
      a1=1.136*exp(zs)
      b1=-0.152*exp(ys)
      du=eqsd(l+1)
      d1=eqsd(l)
      dc=du-d1
      fp=a1*(exp(b1*du)-exp(b1*d1))/(b1*dc)
      if(fp .gt. 1.) fp=1.
      call slbdn(nstp,l,fract)
      psd=tpa*a*fract*fp

      end
  

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                          CC
CC                         RCLMAP                                           CC
CC                                                                          CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine rclmap(ncls,i,numdiv,mxcl,rcel,rco,rd1,rd2,rd3)

c$large:numdiv,rcel

C-This routine takes RCODIV, which has already been read in, and
C- matches it with the corresponding NUMDIV(I).
C-Then the subscript of that NUMDIV (or I) is assigned to RCEL(NCLS)
C- thereby mapping the RCODIV with the "psuedo" corresponding cell.
C-The NUMDIVs are found using a binary search routine.           

C-PARAMETERS:

c  NCLS         = input, total # of cells including divided cells (I)
c  I            = input, do loop index of cell number from loop 1 (I)
c  NUMDIV(ncls) = input, cell # and divisions from nclmap subroutine (I*4)
c  MXCL         = input, max. # cells allowed, used for dimensioning (I)
c  RCO          = input, cell # portion of rcodiv, read from input file (I)
c  RD1          = input, 1st level div. id of rcodiv, read from input (I)
c  RD2          = input, 1st level div. id of rcodiv, read from input (I)
c  RD3          = input, 1st level div. id of rcodiv, read from input (I)
c  RCEL(ncls)   = output, "pseudo" rec. cell obtained by matching rcodiv
c                 with numdiv (I)
c  RCODIV       = within, rec. cell # and divisions as computed from 
c                 RCO,RD1,RD2,RD3 (I*4)
c  MID   = middle of "pseudo" cell range presently being searched (I)

      integer*4 ncls,i,lo,hi,mid,mxcl,rco,rd1,rd2,rd3,
     &          numdiv(mxcl),rcodiv,numb,nd1,nd2,nd3,search
      integer*4 rcel(mxcl)
      save /blk/,/psourc/
      
      rcodiv=rco*1000+rd1*100+rd2*10+rd3

      if(rcodiv.eq.0) then
        rcel(i)=0
        goto 999
      endif

      if(rcodiv.gt.numdiv(ncls)) then
        rcel(i)=ncls+1
        goto 999
      endif

      mid=search(rcodiv,ncls)

      if(mid.gt.0) rcel(i)=mid

999   end
