CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                          CC
CC                                 LOOP2                                    CC
CC                                                                          CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine loop2(awe,chwi,ge,ncls,psnum,qs1,sdsw,setv,spd,ti,
     *                 tk,vi)

	include 'common.blk'

C-------------------------
C  PARAMETER DESCRIPTION
C-------------------------

c The variables have same descriptions as the main program's dictionary
c of variables and definitions.  The variables designated as "input"
c are used and not modified or not used elsewhere

c AWE(5) : input, Area weighted erosion, sum in tons, printout (t/acre)
c CHWI   : input, Channel width going into the cell (ft)
c GE(6)  : input, Gully erosion for particle sixe sum (tons), printout (t/acre)
c QS1    : input, Sediment flow rate into the cell (lbs/sec)
c SDSW(5): input, Particle specific weight (lbs/ft^3)
c SETV(5): input, Settling velocity for each particle class
c SPD(5) : input, Particle diameter (mm)
c TI     : input, Shear stress into cell
c TK(5)  : input, Transport capacity per unit width leaving cell (lbs/sec ft)
c VI     : input, Velocity into the channel (ft/sec)
  
      integer*4 ncls,psnum
      real awe(6),chwi,ge(6),qs1,sdsw(5),setv(5),spd(5),ti,tk(5),vi

C-------------------
C  LOCAL VARIABLES        
C-------------------

c CHWO   : Channel width going into the cell (ft)
c CQS    : Sediment flow rate within cell via overland erosion (lbs/sec)
c D      : Duration of concetrated flow in the cell
c DA     : Drainage area (acres)
c DFD    : Average duration of concentrated flow through the cell (sec)
c DIST   : Travel length through the cell
c FLOW   : Concentrated peak flow rate for the cell (cfs)
c FRACT  : Fraction of the soil particle for the cell erosion 
c GS     : Transport capacity out of the cell (lbs/s)
c I,J    : loop index
c PGE    : Potential gully erosion in the cell (tons)
c QSO    : Sediment flow rate out of the cell (lbs/sec)
c RND    : Function rounds a real number to integer*4  
c ROE    : Runoff (in.), output from TRO subroutine
c RTIME  : Remaining routing time (sec)
c SED1   : Sediment yield t1 for overland flow, t2 for concentrated flow (lbs/s)
c SUMCOD : Sum of COD from all nonfeedlot point sources in a cell (lbs)
c SUMPSF : Function, sums all nonfeedlot point source flow rates in a cell
c SUMN   : Sum of N from all nonfeedlot point sources in a cell (lbs)
c SUMP   : Sum of P from all nonfeedlot point sources in a cell (lbs)
c TCO    : Transport capacity per unit width leaving cell (lbs/sec ft)
c TO     : Shear stress out of cell
c TXIN   : Total sediment available due to overland erosion (lbs)
c VO     : Velocity out of the channel (ft/sec)
c XIN    : Sediment available due to overland erosion (lbs)
c XLNGTH : Length across the cell (ft)
c XOUT   : Sediment leaving the cell (lbs)

      integer*4 i,rnd,j

      real
     *chwo,cqs,d,da,dfd,dist,flow,fract,gs,pge,qso,roe,rtime,
     *sed1,sumcod,sumpsf,sumn,sump,tco,to,txin,vo,xin,xlngth,xout
  
      save /blk/,/psourc/
      write(*,'(a\)')'      ... Loop #2 : Pre-Routing.                '
      psnum=1
  
      do 200 i=1,ncls

C if cell drains into itself then goto 200

      if(rcel(i) .eq. 0) goto 200

      xlngth=sqrt(area(i)*43560.)
      dist=xlngth/2.
      da=ant(i)/10.
      call hydro(da,(cchs(i)),(cro(i)),dist,flow,d)

C add overland flow and flow from all nonfeedlot point sources in cell
C to peak flow from any impoundments in the cell

      df(i)=df(i)+flow+sumpsf(i)
      call tro(area(i),(cchs(i)),(df(i)),dist,roe,d)
      ero(i)=roe
  
C skip to next cell if the cell is not a primary cell
  
      if (pcell(i).gt.0) goto 200

      write(*,117)char(13),i
117   format(a,i4)

C sum all nonfeedlot point source nutrients within primary cell I

      call psnutr(i,d,sumn,sump,sumcod)
      soln(i)=soln(i)+sumn
      solp(i)=solp(i)+sump
      scod(i)=scod(i)+sumcod
      cron(i)=rnd(cron(i)+sumn*100./area(i))
      crop(i)=rnd(crop(i)+sump*100./area(i))
      ccod(i)=rnd(ccod(i)+sumcod*100./area(i))
      call chge((chss(i)),(coef(i)),(df(i)),(cchs(i)),chwo,to,vo)
      dfd=d/2.
      txin=0.
      xout=0.

      do 210 j=1,5
        call slbdn((sltp(i)),j,fract)
        xin=ttllbs(i)*da*fract/area(i)+sedy(i,j)*2000.+sme(i,j)
        cqs=xin/cd(i)
        awe(j)=awe(j)+ttllbs(i)*fract/2000.
        call sdflo(df(i),chwo,dist,setv(j),float(uf(i)),chwi,
     *  qs1,tk(j),sdsw(j),spd(j),ti,vi,to,vo,cqs,qso,tco)
        gs=chwo*tco

        if(qso .lt. gs) qso=gs

        sed1=qso*cd(i)
        cqs=0.
        call sdflo(df(i),chwo,dist,setv(j),float(uf(i)),chwi,
     *  qs1,tk(j),sdsw(j),spd(j),ti,vi,to,vo,cqs,qso,tco)
        gs=chwo*tco

        if(qso .lt. gs) qso=gs

        rtime=dfd-cd(i)
        if(rtime .gt. 0) sed1=sed1+qso*rtime
        sedy(i,j)=sed1/2000.
        pge=(sed1-xin)/2000.
        if(pge .le. 0) pge=0.
        ge(j)=ge(j)+pge
        txin=txin+xin
        xout=xout+sed1
210   continue

      pge=(xout-txin)/2000.
      if(pge .le. 0) pge=0.
      ge(6)=ge(6)+pge
      awe(6)=awe(6)+ttllbs(i)/2000.

200   continue

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             TRO                                                  CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  
      subroutine tro(a,cs,qp,xl,ro,da)

C--SUBROUTINE TO DERIVE THE EQUIVALENT RUNOFF VOLUME LEAVING A
C--CELL WITH AN IMPOUNDMENT IN IT

C PARAMETERS:

c   A  - input, drainage area (acres)
c   CS - input, channel slope (%*10)
c   QP - input, peak flow (cfs)
c   XL - input, flow length (ft)
c   ROLD - old ro, runoff (in.)
c   RO - output, runoff (acre-in)
c   DA - output, flow duration (sec)

      integer*4 cs
      real xs,pow,t1,rold

      save /blk/,/psourc/
      xs=cs/1000.
      pow=1/(.824*a**.0166)
      t1=(xl**2./(a*43560.))**.187
      rold=(qp/8.484*a**(-.7)*xs**(-.159)*t1)**pow
      ro=rold*a
      da=rold*3630.*a/qp

      end
  

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             SUMPSF                                               CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      function sumpsf(i)

C This will add up all the nonfeedlot point source flow for cell I.  If
C there are no point sources in this cell then return zero.

C PARAMETER:

C   I  = input, cell number to check for flow

	include 'common.blk'

C COMMON VARIABLES:

C   PTSNUM = number of the cell for the IIth point source
C   PTSFLW = point source flow
C   LASTPS = number of nonfeedlot point sources in the entire watershed

C CONSTANTS:

C   MAXPS=max # of cells which can have a non-feedlot point source.
c      PARAMETER(MAXPS=20)
c  
c      INTEGER*4 PTSNUM
c      COMMON /PSOURC/PTSNUM(MAXPS),PTSP(MAXPS),PTSN(MAXPS),
c     *PTSC(MAXPS),PTSFLW(MAXPS),LASTPS,PTSP1(MAXPS),PTSN1(MAXPS),
c     *PTSC1(MAXPS)

      integer*4 j
      real psflow

      psflow=0

      do 10 j=1,lastps
        if(i.eq.ptsnum(j)) then
          psflow=psflow+ptsflw(j)
        endif
10    continue

      sumpsf=psflow

      end


