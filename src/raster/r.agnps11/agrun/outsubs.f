cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                        c
c                  opt_output                                            c
c                                                                        c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine opt_output(np,ncls,awe,ge,tadlc,fctr,
     &                      sy,nlc,wtshd,barea,rain,ei)

	include 'common.blk'

c PARAMETERS:

c  awe(5)  : output - passed to "sed_out"
c  factr   : output - passed to "nut_out" and "sed_out"
c  ge(6)   : output - passed to "sed_out"
c  ncls    : input
c  nlc     : input - passed to "nut_out"
c  np      : input
c  sy(6)   : output - passed to "nut_out" and "sed_out"
c  tadlc   : output to "nut_out" and input to "sed_out"

      character*30 wtshd
      integer*4    ncls,np,nlc
      real         awe(6),ge(6),tadlc,fctr,sy(6),barea,rain,ei
      save /blk/,/psourc/



c - point source output to file

      call psrc_out(NP)
  
c - write watershed summary to output file

      call nut_out(np,nlc,tadlc,fctr,sy,wtshd,barea,rain,ei)
      call sed_out(np,awe,ge,tadlc,fctr,sy)
  
C - write soil loss and hydrology info

      call dsl(np,ncls)

C - write nutrient information to file

      call opt_nut(np,ncls)

      end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                        c
c                  opt_nut                                               c
c                                                                        c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine opt_nut(np,ncls)

c print optional nutrient output

	include 'common.blk'

ccccccccccccccccccc
c Parameters:
ccccccccccccccccccc

c np        : (input)  unit number for printing
c ncls      : (input)  # of cells in watershed

      integer*4 np,ncls,ncdiv
      save /blk/,/psourc/

ccccccccccccccccccc
c local variables:
ccccccccccccccccccc

C  ARO     :Summation of runoff out of the cell
C  CELLSD  :Overland erosion for each cell (t/a)
C  CC      :COD yield for the cell in lbs/a*100.  Same as CCOD(I).
C  CN      :Soluble nitrogen in the cell runoff (lbs/a)
C  CP      :Soluble phosphorus in the cell runoff (lbs/a)
C  CSN     :Cell sediment N (lbs/acre)
C  CSP     :Cell sediment P (lbs/acre)
C  DA      :Drainage area (acres)
c  FCTR    :converts lbs/a of nutrient to ppm
c            therefore had to adjust it back for output, used this variable
c  numb    :cell number (used only if cell divisions involved)
c  nd1     :cell division level 1 (used only if cell divisions involved) 
c  nd2     :cell division level 2 (used only if cell divisions involved) 
c  nd3     :cell division level 3 (used only if cell divisions involved)  
c  RPPMC   :soluble COD concentration (ppm)
c  RPPMN   :soluble N concentration (ppm)
c  RPPMP   :soluble P concentration (ppm)
c  RO2     :accum. runoff volume out of the cell (in)
c  SDN     :sediment attached N (lbs/a)
c  SDP     :sediment attached P (lbs/a)
c  SDTPA   :sediment yield (t/acre)
c  SY(6)   :sediment yield for each particle class and total (tons)
c  TSCOD   :total soluble COD (lbs/a)
c  TSN     :total soluble N (lbs/a)
c  TSP     :total soluble P (lbs/a)

      integer*4 i, ii, l, j, rnd, numb, nd1, nd2, nd3
      real aro, cellsd, cc, cn, cp, csn, csp, da, fctr, rppmn,
     &     rppmp, rppmc, ro2, sdn, sdp, sdtpa, sy(6), tscod, tsn, tsp
      character*132 buff

      write(np,'(a)') 'NUTRIENT'

      write(*,'(a\)')'      ... Nutrient Output Being Created.        '
      l=0

      do 710 ii=1,ncls

        i=ii
        write(*,713)char(13),i
713     format(a,i4)

        sy(6)=0.

        do 715 j=1,5
          sy(6)=sy(6)+sedy(i,j)
715     continue
  
        da=adrain(i)+area(i)

C  check for a division by zero with the occurrence of a dead end cell
c  which had no cells draining into to it.

        if(rcel(i) .eq. 0) then
          ro2=0.0
          fctr=0.0
        else
          aro=smero(i)+ero(i)
          ro2=aro/da
          fctr=10.**6./(ro2*3630.*62.4)
        endif

        sdtpa=sy(6)/da
        call sdnut(sdtpa,(sltp(i)),sdn,sdp)

C  CALC. CELL-SEDIMENT-N AND P

        cellsd=ttllbs(i)/(2000.*area(i))
        call sdnut(cellsd,(sltp(i)),csn,csp)
        tsn=soln(i)/da
        tsp=solp(i)/da
        tscod=scod(i)/da
        rppmn=tsn*fctr
        rppmp=tsp*fctr
        rppmc=tscod*fctr
        cn=cron(i)/100.
        cp=crop(i)/100.
        cc=ccod(i)

c write nutrient output to *.DSP file ...

       
        call realnum(numb,nd1,nd2,nd3,numdiv(i))

        write(buff,720) numb,nd1,nd2,nd3,rnd(da),csn,sdn,cn,tsn,
     &                    rnd(rppmn)

	call putbuff(buff,np)
	write(buff,725) csp,sdp,cp,tsp,rnd(rppmp),
     &                    cc,tscod,rnd(rppmc)

	call putbuff(buff,np)
720     format(i10,1x,3i1,i10,2f10.2,2f10.2,i10)
725     format(4f10.2,i10,2f10.2,i10)


710   continue

      write(np,'(a)') '****'

888   end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                        c
c     Detailed Soil Loss Output                                          c
c                                                                        c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  
      subroutine dsl(np,ncls)

	include 'common.blk'

c PARAMETERS:

c  ncls          : input
c  np            : input

      integer*4 np,ncls
      save /blk/,/psourc/

c LOCAL VARIABLES:

C  ARO         :Summation of runoff out of the cell
C  ASED        :Summation of sediment delivered to the cell channel (tons)
C  BUFF        :temp. write buffer to edit graphics output before writing
C  DA          :Drainage area (acres)
C  DEP         :Deposition in the cell (%)
C  DIVIDE      :Sediment delivered to channel or sediment yield, whichever is >
C  EROS        :Cell overland erosion (tons/acre)
C  ES          :Eroded sediment in a particle size from a cell (t/a)
c  FCTR        :converts lbs/a of nutrient to ppm
C  FRACT       :Fraction of the soil particle for the cell erosion
c  numb        :cell division, base cell number
c  nd1         :cell division level 1
c  nd2         :cell division level 2
c  nd3         :cell division level 3
c  OUTERO      :ERO(I) was adjusted based on cell area in subroutine tro, 
c                 this is used to adjust it back for output
C  OUTLBS      :Sediment leaving the cell (lbs)
C  PRGA        :Runoff generated above the cell (%)
C  RO1         :Accumulated Runoff volume into the cell (in)
c  RO2         :accum. runoff volume out of the cell (in)
C  SGW         :Sediment generated within the cell (tons)
c  SY(6)       :sediment yield for each particle class and total (tons)
c  TSEE_C_SOIL :temporary variable holds whether to see condensed or not
C  TTLDP       :Deposition in the cell (%)
C  UY(6)       :Upstream sediment yield (tons)
C  XINLBS      :Sediment available in the cell (lbs)

      integer*4 i,ii,j,rnd,numb,nd1,nd2,nd3
      real      ro1,da,prga,aro,sgw,uy(6),sy(6),ased,ttldp,divide,
     &          eros,fract,xinlbs,outlbs,dep,es,outero,ro2
      character*132 buff

      write(np,'(a)') 'SOIL_LOSS'
      write(*,'(a\)')'      ... Soil Loss Output Being Created.       '

      do 600 ii=1,ncls

        i=ii

        write(*,609)char(13),i
609     format(a,i4)

      if(adrain(i).eq.0) then
        ro1=0.0
      else
        ro1=smero(i)/adrain(i)
      endif

      da=adrain(i)+area(i)

      if(rcel(i).eq.0) then
        ro2=0.0
        prga=0.
      else
        aro=smero(i)+ero(i)
        ro2=aro/da
        prga=smero(i)/aro*100.
      endif

      sgw=ttllbs(i)/2000.
      uy(6)=0.
      sy(6)=0.

      do 610 j=1,5
        uy(j)=sme(i,j)/2000.
        uy(6)=uy(6)+uy(j)
        sy(j)=sedy(i,j)
        sy(6)=sy(6)+sy(j)
610   continue

      ased=uy(6)+ttllbs(i)/2000.
      ttldp=0.

      if(coef(i).eq.13 .and. ased.lt.sy(6)) sy(6)=ased

      divide=ased
      if(sy(6).gt.ased) divide=sy(6)

      if(divide.gt.0.0) then
        ttldp=(ased-sy(6))/divide*100.
      else
        ttldp=0.0
      endif

      if(abs(ttldp).lt.0.5) ttldp=0.
      eros=ttllbs(i)/(2000.*area(i))

C detailed summary
  
      do 620 j=1,6

      call slbdn((sltp(i)),j,fract)
      xinlbs=uy(j)*2000.+ttllbs(i)*fract
      outlbs=sy(j)*2000.
      dep=0.

      if(coef(i).eq.13 .and. outlbs.ge.xinlbs) then
        outlbs=xinlbs
        sy(j)=outlbs/2000.
      endif

      divide=xinlbs
      if(outlbs.gt.xinlbs)divide=outlbs
      if(divide.gt.0.0) then
        dep=(xinlbs-outlbs)/divide*100.
      else
        dep=0.0
      endif
      if( abs(dep).lt.0.5) dep=0.
      sgw=ttllbs(i)*fract/2000.
      es=eros*fract


      if (j .ne. 1) then

        write(buff,625) es,uy(j),sgw,sy(j),rnd(dep)
625     format(4f10.2,i12)
        call putbuff(buff,np)

      else

        outero=ero(i)/area(i)

        call realnum(numb,nd1,nd2,nd3,numdiv(i))
        write(buff,635) numb,nd1,nd2,nd3,rnd(da),outero,ro1,uf(i),
     *                    ro2,rnd((df(i))),prga
634     format(i3,1x,3i2,i2,f6.2,f4.2,i4,f4.2,i5,f5.1)
635     format(i10,1x,3i1,i10,2f10.2,i10,f10.2,i10,f10.1)
        call putbuff(buff,np)

        write(buff,640) es,uy(j),sgw,sy(j),rnd(dep)
640     format(4f10.2,i10)
        call putbuff(buff,np)


      endif

620   continue

600   continue

      write(np,'(a)') '****'

      end



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc                                                                   cc
cc                  psrc_out                                         cc
cc                                                                   cc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine psrc_out(np)

	include 'common.blk'

c PARAMETERS:

c  NP       : output device 

c LOCAL VARIABLES:

C  buff          : Temp. buffer to edit graphics output before outputing it
c  control       : det. if this is a total line or single feedlot line
c  header_printed: becomes true when the feedlot header is printed
c  i, l          : loop variables
c  numb          : holds the actual cell number without cell division
c  nd1           : holds the 1st cell division level
c  nd2           : holds the 2nd cell division level
c  nd3           : holds the 3rd cell division level
c  FDLOTN   :\
c  FDLOTP   : > see do_pntsrc routine (same variables were written out
c  FDCOD    :/  to the scratch file there)
c  LN       :\
c  CELL     : \
c  NFRATE   :  \
c  DPNC     :   \
c  DPPC     :    > see FDLOT routine (same variables were written out
c  DPOC     :   /  to the scratch file there and are read in here)
c  DPNM     :  /
c  DPPM     : /
c  DPOM     :/

      character*132 buff
      real      dpnc,dppc,dpoc,dpnm,dppm,dpom,fdlotn,fdlotp,fdcod
      integer*4 i, control, ln, cell, nfrate, np, l,
     &          ncdiv, numb, nd1, nd2, nd3, numproc


       save /blk/,/psourc/
c print out feedlot point sources if needed (read from scratch file)

      l=1
      numproc=0

      write(np,'(a)') 'FEEDLOT'
      write(*,'(a\)')'      ... Feedlot Output Being Created.         '

10    read(99,*) control

      numproc=numproc+1
      write(*,402)char(13),numproc
402   format(a,i4)
       	
      if (control.eq.1) then
         read(99,*)    ln,cell,dpnc,dppc,dpoc,dpnm,dppm,dpom,nfrate
         
         call realnum(numb,nd1,nd2,nd3,numdiv(cell))
         write(buff,401) numb,nd1,nd2,nd3,dpnc,dppc,dpoc,dpnm,dppm,
     &   dpom,nfrate
401      format(i10,1x,3i1,6f10.3,i10)
         call putbuff(buff,np)

c if this is a total line

      elseif (control.eq.2) then
        read(99,*) fdlotn,fdlotp,fdcod
c        if (cell.eq.npc(l)) then
          write(buff,195) 'TOTL',fdlotn,fdlotp,fdcod
195       format(a4,1x,3f10.3)
          call putbuff(buff,np)
c        endif
      endif

c if the control=0 then we are at EOF else get next one
      if (control.gt.0) goto 10

c close (and automatically erase) the feedlot scratch file
20    rewind(99)

      write(np,'(a)') '****'

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc                                                                   cc
cc                  putbuff                                          cc
cc                                                                   cc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine putbuff(buff,np)

c This subroutine will check the numbers written to 'buff' to see if
c FORTRAN has put any numbers there with a space before the decimal 
c point.  This is needed for reading with the Turbo Pascal graphics
c program. All excess spaces are removed between numbers

      character*132 buff,buff2
      integer np,position,i,j,k
      save /blk/,/psourc/

10    position=index(buff,' .')
      if (position.gt.0) then
        buff(position:position)='0'
        goto 10
      endif

15    position=index(buff,'. ')
      if (position.gt.0) then
        buff(position+1:position+1)='0'
        goto 15
      endif

20    position=index(buff,' -.')
      if (position.gt.0) then
        buff(position:position+1)='-0'
        goto 20
      endif

c delete all but one space bewteen groups of characters
c assumes usable string length to start at 80 characters

      j=1

      do 25 i=1,132

      buff2(i:i)=' '

      if (buff(i:i).ne.' ') then
        buff2(j:j)=buff(i:i)
        j=j+1
      else
        if (buff(i+1:i+1).ne.' ' .and. j.ne.1) then
          buff2(j:j)=' '
          j=j+1
        endif
      endif

25    continue

      buff2(j:j)=' '

c check for last non-blank character & only write that many chars. to the
c graphics output file

      do 30 i=80,2,-1
        if (buff2(i:i).ne.' ') goto 40
30    continue

40    write(np,'(A)')buff2(1:i)
      end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc                                                                   cc
cc                  nut_out                                          cc
cc                                                                   cc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine nut_out(np,nlc,tadlc,fctr,sy,wtshd,barea,rain,ei)

c write to output device the initial watershed nutrient info.

	include 'common.blk'

c PARAMETERS:
c------------

c   fctr    : output
c   nlc     : input          
c   np      : input
c   sy(6)   : output 
c   tadlc   : output - total area draining out of last cell

      character*30 wtshd
      integer*4 np,nlc
      real tadlc,fctr,sy(6),rain,ei,barea
      save /blk/,/psourc/

c LOCAL VARIABLES:

c  RO2     :accum. runoff volume out of the cell (in)
c  RPPMC   :soluble COD concentration (ppm)
c  RPPMN   :soluble N concentration (ppm)
c  RPPMP   :soluble P concentration (ppm)
c  SDN     :sediment attached N (lbs/a)
c  SDP     :sediment attached P (lbs/a)
c  SDTPA   :sediment yield (t/acre)
c  TSN     :total soluble N (lbs/a)
c  TSP     :total soluble P (lbs/a)
c  TSCOD   :total soluble COD (lbs/a)

      integer*4 rnd,J
      real ro2,sdtpa,sdn,sdp,tsn,tsp,tscod,rppmn,rppmp,rppmc
      character*132 buff

* total area draining out of last cell
      
      tadlc = adrain(nlc)+area(nlc)
      ro2 = (smero(nlc)+ero(nlc))/tadlc

      sy(6) = 0.0
      do 410 j=1,5
        sy(j) = sedy(nlc,j)
        sy(6) = sy(6)+sy(j)
410   continue

      sdtpa = sy(6)/tadlc
      call sdnut(sdtpa,sltp(nlc),sdn,sdp)
      tsn = soln(nlc)/tadlc
      tsp = solp(nlc)/tadlc
      tscod = scod(nlc)/tadlc
      fctr = 10.**6./(ro2*3630.*62.4)
      rppmn = tsn*fctr
      rppmp = tsp*fctr
      rppmc = tscod*fctr

      write(np,'(a)') 'INITIAL'
      write(*,'(a\)')'      ... Initial Output Being Created.         '
      write(*,'(a\)') char(13)

      write (buff,403) wtshd
403   format(a30)
      call putbuff(buff,np)

      call realnum(numb,nd1,nd2,nd3,numdiv(nlc))

      write(buff,400) rnd(tadlc),barea,rain,rnd(ei),
     &                numb,nd1,nd2,nd3,ro2,rnd(df(nlc))

401   format(i4,2f6.2,i4,i4,1x,3i1,f6.1,i7,i4)
400   format(i10,2f10.2,i10,i10,1x,3i1,f10.1,i10)
     
      call putbuff(buff,np)
     
      write(buff,415) sdn,tsn,rppmn,sdp,tsp,rppmp,tscod,rnd(rppmc)

      call putbuff(buff,np)

415   format(7f10.2,i10)

      write(np,'(a)') '****'

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc                                                                   cc
cc                  sed_out                                          cc
cc                                                                   cc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine sed_out(np,awe,ge,tadlc,fctr,sy)

c PARAMETERS:

c  NP     : input
c  AWE(6) : input
c  GE(6)  : input
c  FCTR   : input
c  SY     : input 
c  TADLC  : input

      integer*4 np
      real      awe(6), ge(6), tadlc, fctr, sy(6)
      character*132 buff
      save /blk/,/psourc/

c LOCAL VARIABLES:

C  AWE2    :Local variable used for printing, awe()/area 
C  AWY     :Area Weighted yeild (tons/acre)
C  CONC    :Concentration of the sediment in the runoff (ppm)
C  DR      :Delivery ratio (%)
C  EFRCT   :Fraction of particle size in the entire yield
C  ER      :Enrichment ratio of yield fraction to eroded fraction
C  GE2     :Local variable used for printing, ge()/area
c  J       :loop variable

      integer*4 J, rnd
      real      dr, efrct, er, conc, awy, awe2, ge2

	include 'common.blk'

      write(np,'(a)') 'SEDIMENT'
      write(*,'(a\)')'      ... Sediment Output Being Created.        '
      write(*,'(a\)') char(13)

      do 415 j=1,6

        awe2=awe(j)
        ge2 =ge(j)
        dr  =(sy(j)/(awe(j)+ge(j)))*100.
  
C check for division by zero

        if (sy(6).ne.0.0) then
          efrct=sy(j)/sy(6)
        else
          efrct=0
        endif

        if (awe(j).ne.0.0 .and. awe(6).ne.0.0) then
          er=efrct/(awe(j)/awe(6))
        else
          er=0
        endif
  
        conc=fctr*sy(j)*2000./tadlc
        awy =sy(j)/tadlc
        awe2=awe(j)/tadlc
        ge2 =ge(j)/tadlc

        write(buff,420) awe2,ge2,rnd(dr),rnd(er),conc,awy,sy(j)
420     format(2f10.2,2i10,f14.2,f10.2,f11.1)
        call putbuff(buff,np)

415   continue

      write(np,'(a)') '****'

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             SDNUT                                                CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine sdnut(sedeu,nstp,sedn,sedp)

C--SUBROUTINE TO CALCULATE SEDIMENT NUTRIENT YIELDS
C--TAKEN OUT OF THE EROSION MODEL CREAMS

C PARAMETERS:

c  SEDEU - input, sediment yld. (tons/acre)
c  NSTP  - input, soil type number
c  SEDN  - output, sediment N (lbs/acre)
c  SEDP  - output, sediment P (lbs/acre)

C LOCAL VARIABLES:

c  TF    - adjustment factor to correct sediment adsorbed nutrient ERs
c          for soil type
c  AN,BN,AP,BP - constants in the erichment ratio equation: ER=A*SED**B*TF
c  SOILN - N concentration in soil (.001)
c  SOILP - P concentration in soil (.0005)
c  SED   - sediment yield (kg/h)
c  ERN   - N enrichment ratio
c  ERP   - P enrichment ratio

      integer nstp
      real tf(5),an,bn,ap,bp,soiln,soilp,sed,ern,erp 

      data tf/0.85,1.00,1.15,1.5,  1.15/
      data an,bn,ap,bp,soiln,soilp/7.4,-.2,7.4,-.2,.001,.0005/

      save /blk/,/psourc/
      sed=sedeu*2241.6
      sedn=0.
      sedp=0.

      if(sed .eq. 0) goto 10

      ern=an*sed**bn*tf(nstp)
      sedn=soiln*sed*ern
      erp=ap*sed**bp*tf(nstp)
      sedp=soilp*sed*erp
      sedn=sedn*2.205/2.471
      sedp=sedp*2.205/2.471

10    continue

      end
  

