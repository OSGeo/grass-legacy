      program agnps
  
C  AGNPS is a program designed to predict nutrient & sediment runoff
C  from agricultural lands.  Originally written by David Bosch.
C  Updated & altered by Larry Winkelman & Cully Hession.
C  UPDATE DATE ----- <08/03/88>
C  VERSION 3.0-PC

C  Updated again to work with St.Paul AGNPS-PC Ver. 2.0
C  by L. Bruce Lucord -

*  this version allows the division of cells into to 1/4,1/16,1/64
*  this was developed as part of the site-specific sediment deposition project
*  by Cully Hession (4/11/88)

*  the division requires a modified input file which extends the fields for 
*  both the cell number and receiving cell number to include the cell division 
*  indicators (3 integers)

      integer*4
     *    bncls, impfmt, inp, ncdiv,ncls,nlc,np,
     *    pointfmt,prwhere,psnum
      real
     *    awe(6),barea, chwi, ei, fctr,ge(6), qs1,
     *    rain,ro2,rppmc,rppmn,rppmp,sdn,sdp,sdsw(5),sdtpa,
     *    setv(5),shpf(3),spd(5),sy(6),tadlc,ti,tk(5),tscod,
     *    tsn,tsp,vi

      logical see_inp, again
  
      character*80 file,ofile
      character*50 descri
      character*4 ptcl(6),soil(5)
      character*30 wtshd

	include 'common.blk'

      data ptcl/'CLAY','SILT','SAGG','LAGG','SAND','TOTL'/
      data soil/'SAND','SILT','CLAY','PEAT','WATR'/
      data setv/.0000102,.000263,.00125,.0542,.0759/,shpf/1.,1.3,.88/
      data tk/.006242,.006053,.012478,.016631,.006053/
      data sdsw/162.37,165.49,112.41,99.92,165.49/
      data spd/.002,.010,.035,.500,.200/
      data inp/2/,np/3/

      save /blk/,/psourc/


C VARIABLES & ARRAYS:
C           -  arrays with (I) as a dimension are (1..MXCL)
C           -  cp=cell parameter   wp=watershed parameter
C  AGAIN   :Run the model again or not
C  ANT(I)  :Area (acres) which is not terraced * 10 (cp)
C  AREA(I) :area (acres) of each cell (wp)
C  ASP(I)  :Cell Aspect  cp
C  AWE(5)  :Area Weighted Erosion, sum in tons, printout in t/acre
c  BAREA   :This takes place of old AREA and is read in from 1st input line
c  BNCLS   :This replaces old NCLS and is read in 1st line, it represents
c               the number of base cells before divisions
C  CCHS(I) :Channel slope (%*10)  cp
C  CCOD(I) :Chemical Oxygen Demand yield for the cell (lbs/acre*100)
C  CD(I)   :Cell Duration of overland flow (sec)
C  CHSS(I) :Channel side slope (%*10)  cp
C  CHWI    :Channel width going into the cell (ft)
C  COEF(I) :Mannings roughness coefficiant for the channel *1000  cp
C  CRO(I)  :Cell overland runoff (in)
C  CRON(I) :Soluble Nitrogen in the cell runoff (lbs/a*100)  cp
C  CROP(I) :Soluble Phosphorous in the cell runoff (lbs/a*100) cp
C  DESCRI  :Description of what the cell parameters are derived for.
C  DF(I)   :Concentrated flow rate downstream from the cell (cfs)
C  DIST1(I):Total length of the maximum flow path up to the cell (ft)
C  DIST2(I):Total length of the maximum flow path out of the cell (ft)
C  EI      :Energy Intensity value for the storm
C  ERO(I)  :Equivalent runoff for the cell (in)
C  FCTR    :Converts lbs/a of nutrient to ppm.
C  FILE    :Data File name
C  GE(6)   :Gully erosion for particle size sum (tons), printout (t/a)
C  IMPFMT  :Impoundment factor format: across or down?
C  INP     :Fortran unit number of the input file
C  LASTPS  :Number of nonfeedlot point sources in watershed.
C  MAXPS   :Max. number of point sources in a watershed. Constant.
C  MXCL    :Max. number of cells in a watershed. Constant.
C  MXCL1   :Max. number of cells in a watershed plus one. Constant.
C  N2(I)   :Counter - number of cells draining directly into cell I
c  NCDIV   :indicates if cell divisions will occur or input is in 
c           cell division type format
c  NCLS    :calculated in subroutine NCLMAP and represents number of "pseudo"
c               cells including divided cells
C  NLC     :Number of the watershed outlet cell
C  NP      :Unit number of the printout
c  NUMDIV(I):7 digit # the 1st 4 represent base cell # and the last 3i
c            represent the cell division levels
C  OFILE   :Output filename
C  PCELL(I):Contains the number of cells which drain into each given
C           cell. If PCELL(I)=0 then I is a primary cell.
C  POINTFMT:point source format: old way with feedlots only or new format?
C  prwhere :where the output is sent to (1=screen,2=printer,3=file)
C  PSNUM   :Counter for the number of point sources in the watershed
C  PTCL(6) :Particle class name
C  PTSC()  :Nonfeedlot point source COD (ppm)
C  PTSC1() :Nonfeedlot point source COD (lbs)
C  PTSFLW():Nonfeedlot point source peak flow rate (cfs)
C  PTSN()  :Nonfeedlot point source N (ppm)
C  PTSN1() :Nonfeedlot point source N (lbs)
C  PTSNUM():Cell # where this nonfeedlot point source is located
C  PTSP()  :Nonfeedlot point source P (ppm)
C  PTSP1() :Nonfeedlot point source P (lbs)
C  QS1     :Sediment flow rate into the cell (lbs/s)
C  RAIN    :Storm rainfall (in)
C  RCEL(I) :Receiving pseudo cell number for the cells outflow  cp
C  RO2     :Accumulated Runoff volume out of the cell (in)
C  RPPMC   :Soluble COD concentration (ppm)
C  RPPMN   :Soluble N concentration (ppm)
C  RPPMP   :Soluble P concentration (ppm)
C  SCOD(I) :Soluble COD leaving the cell in the runoff (lbs)
C  SDN     :Sediment attached N (lbs/a)
C  SDP     :Sediment attached P (lbs/a)
C  SDSW(5) :Particle specific weight (lbs/ft^3)
C  SDTPA   :Sediment yield (t/a)
C  SEDY(I,5):Sediment yield for each cell and particle size (tons)
C  see_inp :Flag whether to print cell parameters
C  SETV(5) :Settling velocity for each particle class
C  SHPF(3) :Slope shape factor to adjust for irregular slopes
C  SLTP(I) :Cell soil type number  cp
C  SME(I,5):Summation of the available sediment in the cell (lbs)
C  SMERO(I):Summaton of the effective runoff generated above the cell
C  SMRO1(I):Summation of the total runoff entering the cell
C  SOIL(4) :Major soil texture name
C  SOLN(I) :Soluble N yield from the cell (lbs)
C  SOLP(I) :Soluble P yield from the cell (lbs)
C  SPD(5)  :Particle diameter (mm)
C  SY(6)   :Sediment yield for each particle class and total (tons)
c  TADLC   :Total area draining out of last cell
C  TI      :Shear stress into cell
C  TK(5)   :Transport capacity factors. Constants.
C  TSCOD   :Total soluble COD (lbs/a)
C  TSN     :Total soluble N (lbs/a)
C  TSP     :Total soluble P (lbs/a)
C  TTLLBS(I):Eroded Sediment from overland runoff in the cell (lbs)
C  UF(I)   :Upstream concentrated flow (cfs)
C  VI      :Velocity into the channel (ft/sec)
C  WTSHD   :Watershed name.


      call infiles(file,ofile,inp,np)


      call readheader(inp,wtshd,barea,bncls,rain,ei,ncdiv,descri,mxcl)




      call nclmap(barea,ncls,area,numdiv,mxcl,impfmt,pointfmt,inp)


      call varinit(ge,awe,chwi,qs1,ti,vi,nlc,psnum,ncls)


C  *****RUN MODEL*****


      call loop1(ei,ge,inp,ncls,nlc,pointfmt,psnum,rain,shpf)
  

      call loop2(awe,chwi,ge,ncls,psnum,qs1,sdsw,setv,spd,ti,tk,vi)  
  

      call loop3(awe,ge,ncls,sdsw,setv,spd,tk)  


C  *****PRINT OUT*****


      call opt_output(np,ncls,awe,ge,tadlc,fctr,
     &                sy,nlc,wtshd,barea,rain,ei)
      

      end
