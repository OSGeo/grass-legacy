CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                          CC
CC                                 LOOP3                                    CC
CC                                                                          CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine loop3(awe,ge,ncls,sdsw,setv,spd,tk)

	include 'common.blk'

C-------------------------
C  PARAMETER DESCRIPTION
C-------------------------

c The variables have same descriptions as the main program's dictionary
c of variables and definitions.  The variables designated as "input"
c are used and not modified or not used elsewhere

c   AWE    input - area weighted erosion, sum in tons, print in t/acre
c   GE     input - gully eros. for particle size sum (tons), print t/acre
c   NCLS   input - number of cells in the watershed
c   SDSW   input - particle specific weight (lbs/ft^3)
c   SETV   input - settling velocity for each particle class
c   SPD    input - particle diameter (mm)
c   TK     input - transport capacity factors (constants)

      integer*4 ncls
      real      awe(6), ge(6), sdsw(5), setv(5), spd(5), tk(5)

      save /blk/,/psourc/
C------------------
C LOCAL VARIABLES: 
C------------------

c I,J,K   : loop variables
c A1      : drainage area into the cell (acres)
c A2      : drainage area out of the cell (acres)
c CHWI    : channel width going into the cell (ft)
c CHWO    : channel width going out of the cell (ft)
c CQS     : sediment flow rate within cell via overland eros. (lbs/s)
c D1      : duration of concentrated flow entering the cell (sec)
c D2      : duration of concentrated flow leaving the cell (sec)
c DFD     : ave. duration of concentrated flow thru the cell (sec)
c DSTF    : path lenght factor
c DST     : travel distance across the cell
c FRACT   : fraction of the soil particle for the cell erosion
c GS      : transport capacity out of the cell (lbs/s)
c M       : receiving cell of cell "I"
c PGE     : potential gully erosion in the cell (tons)
c Q1      : peak concentrated flow entering the cell (cfs)
c Q2      : peak concentrated flow leaving the cell (cfs)
c QS1     : sediment flow rate into the cell (lbs/s)
c QSO     : sediment flow rate out of the cell (lbs/s)
c RO1     : accumulated runoff vol. into the cell (in)
c RO2     : accumulated runoff vol. out of the cell (in)
c RTIME   : remaining routing time (sec)
c SED1    : sediment yield t1 for ovrlnd flow, t2 for conc. flow (lbs/s)
c SUMCOD  : sum of COD from all nonfeedlot point sources in a cell (lbs)
c SUMN    : sum of N from all nonfeedlot point sources in a cell (lbs)
c SUMN    : sum of P from all nonfeedlot point sources in a cell (lbs)
c TCO     : transport capacity per unit width leaving cell (lbs/s ft)
c TI      : shear stress into cell
c TLNG    : flow length across the cell in feet
c TO      : shear stress out of cell
c TXIN    : total sediment available due to overland erosion (lbs)
c VI      : velocity into the channel (ft/sec)
c VO      : velocity out of the channel (ft/sec)
c XIN     : sediment available due to overland erosion (lbs)
c XLNGTH  : length across the cell (ft)
c XOUT    : sediment leaving the cell (lbs)
c XTOP    : sediment arriving into the cell at the inlet (lbs)
  
      integer*4 i,rnd,j,k,m

      real
     *a1,a2,chwi,chwo,cqs,d1,d2,dfd,dstf,dst,fract,gs,pge,q1,q2,qs1,qso,
     *RO1,RO2,RTIME,SED1,SUMCOD,SUMN,SUMP,TCO,TI,TLNG,TO,TXIN,VI,VO,XIN,
     *xlngth,xout,xtop

      save /blk/,/psourc/

 
      write(*,'(a\)') '      ... Loop #3 : Final Routing.             '

      do 300 k=1,ncls
      i=k

C If RCEL(I) = 0 then the cell drains into itself

      if(rcel(i) .eq. 0) goto 300
  
C Skip routing loop if I is not a primary cell

      if(pcell(i).gt.0) goto 300
  
      write(*,117)char(13),i
117   format(a,i4)

      xlngth=sqrt(area(i)*43560.)

      dist2(i)=xlngth/2.
  
C ************ START OF ROUTING LOOP ************
  
444   m=rcel(i)
  
C Check if outlet cell or if it drains into itself (M=0)
  
      if(m .gt. ncls .or. m .eq. 0) goto 300

      call path((asp(i)),(asp(m)),dstf)
      dst=dist2(i)+dstf*xlngth

      if(dst .ge. dist2(m)) then
        dist2(m)=dst
        dist1(m)=dist2(i)
      endif

      smro1(m)=smro1(m)+cro(i)+smro1(i)
      smero(m)=smero(m)+ero(i)+smero(i)
      soln(m)=soln(m)+soln(i)
      solp(m)=solp(m)+solp(i)
      scod(m)=scod(m)+scod(i)

      do 310 j=1,5
        sme(m,j)=sme(m,j)+sedy(i,j)*2000.
310   continue

c adrain keeps track of drainage area above cell

      adrain(m)=adrain(m)+adrain(i)+area(i)

      n2(m)=n2(m)+1

C Compare # cells coming into M, if there are still some more cells that
C drain in then skip now and route M later

      if (pcell(m) .ne. n2(m)) goto 300

      a1=adrain(m)
      a2=adrain(m)+area(m)

      xout=0.
      xtop=0.
      txin=0.

      ro1=smero(m)/a1
      ro2=(smero(m)+ero(m))/a2

      call hydro(a1,(cchs(m)),ro1,(dist1(m)),q1,d1)
      uf(m)=q1

      if(rcel(m) .eq. 0) then
        soln(m)=0.
        solp(m)=0.
        scod(m)=0.
        goto 300
      endif

      call hydro(a2,(cchs(m)),ro2,(dist2(m)),q2,d2)

C add in nutrients from all point sources in cell M

      call psnutr(m,d2,sumn,sump,sumcod)
      soln(m)=soln(m)+sumn
      solp(m)=solp(m)+sump
      scod(m)=scod(m)+sumcod
      cron(m)=rnd(cron(m)+sumn*100./area(m))
      crop(m)=rnd(crop(m)+sump*100./area(m))
      ccod(m)=rnd(ccod(m)+sumcod*100./area(m))
      df(m)=q2
      dfd=(d1+d2)/2.
      call chge((chss(m)),(coef(m)),q1,(cchs(m)),chwi,ti,vi)
      call chge((chss(m)),(coef(m)),q2,(cchs(m)),chwo,to,vo)
      tlng=dist2(m)-dist1(m)

      do 320 j=1,5
        call slbdn((sltp(m)),j,fract)
        qs1=sme(m,j)/dfd
        xin=ttllbs(m)*ant(m)*fract/(area(m)*10.)+sedy(m,j)*2000.
        cqs=xin/cd(m)
        awe(j)=awe(j)+ttllbs(m)*fract/2000.
        call sdflo(q2,chwo,tlng,setv(j),q1,chwi,qs1,tk(j),sdsw(j),
     *             spd(j),ti,vi,to,vo,cqs,qso,tco)
        gs=chwo*tco
        if (qso .lt. gs) qso=gs
        sed1=qso*cd(m)
        cqs=0.
        call sdflo(q2,chwo,tlng,setv(j),q1,chwi,qs1,tk(j),sdsw(j),
     *             spd(j),ti,vi,to,vo,cqs,qso,tco)
        gs=chwo*tco
        if (qso .lt. gs) qso=gs
        rtime=dfd-cd(m)
        if (rtime .ge. 0) sed1=sed1+qso*rtime
        sedy(m,j)=sed1/2000.
        pge=(sed1-(xin+sme(m,j)))/2000.
        if(pge .le. 0) pge=0.
        ge(j)=ge(j)+pge
        txin=txin+xin
        xout=xout+sed1
        xtop=xtop+sme(m,j)
320   continue

      pge=(xout-(xtop+txin))/2000.
      if(pge .le. 0) pge=0.
      ge(6)=ge(6)+pge
      awe(6)=awe(6)+ttllbs(m)/2000.
      i=m

      goto 444
  
C END OF ROUTING LOOP
  
300   continue

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             PATH                                                 CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine path(kin,kout,adjs)

C--LENGTH OF FLOW PATH

C-PARAMETERS:

C   KIN  = input, aspect into cell
C   KOUT = input, aspect leaving cell
C   ADJS = output, path length factor

      integer kin,kout,idiff
      real pi
      save /blk/,/psourc/

      pi=3.1415927
      idiff=iabs(kin-kout)
      if(idiff .eq. 4) adjs=0
      if(idiff .eq. 1 .or. idiff .eq. 7) adjs=1/cos(atan(.5))
      if(idiff .eq. 3 .or. idiff .eq. 5) adjs=cos(pi/4.)
      if(idiff .ne. 0) goto 10
      adjs=1/cos(pi/4.)
      if(mod(kout,2) .eq. 1) adjs=1.0

10    if(idiff .ne. 2 .and. idiff .ne. 6) goto 20

      adjs=cos(pi/4.)
      if(mod(kout,2) .ne. 1) adjs=1/adjs

20    continue

      end
  

