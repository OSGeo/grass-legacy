CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                          CC
CC                             NCLMAP                                       CC
CC                                                                          CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine nclmap(barea,ncls,area,numdiv,mxcl,
     &                  impfmt,pointfmt,inp)

c$large:numdiv

C- This routine goes through the input file reading only the cell number
C- and divisions for each line.  
C- A counter will determine how many actual cells there are (including 
C- divisions).
C- The area of each cell is determined based on the level of division.

C-PARAMETERS:

c  BAREA        = input, base area of cells (R)
c  NCLS         = output, actual # of cells including divisions (I)
c  AREA(ncls)   = output, adjusted area for each NCLS based on divisions (R)
c  NUMDIV(ncls) = output, cell # and divisions computed from numb,nd1,nd2,nd3
c  MXCL         = input, max. # cells allowed, used for dimension (I)
c  IMPFMT       = output
c  POINTFMT     = output
c  INP          = input

C-LOCAL VARIABLES:

c  NUMB  = cell number portion of numdiv, read from input file (I)
c  ND1   = first level division ID of numdiv, read from input file (I)
c  ND2   = second level division ID of numdiv, read from input file (I)
c  ND3   = third level division ID of numdiv, read from input file (I)
c  PSID  = # of point sources in the cell
c  IMPF  = impoundment factor (# of ponds in terrace system)
c  PSCODE= point source code: 1=nonfeedlot p.s.  2=feedlot p.s.
c  BUFF  = temporary holder for a line of characters off the input file
c  FERT  = fertilizer level indicator

      integer*4 bncls,ncls,mxcl,numb,nd1,nd2,nd3,i,ncdiv,psid,impf,
     &          numdiv(mxcl),pscode,k,j,pointfmt,fert
      real barea,area(mxcl)
      character*78 buff

      save /blk/,/psourc/
      impfmt=0
      pointfmt=0
      

      i=1

      write(*,'(a\)')'      ... Scanning Input File.                  '

10    read(inp,105,end=999) numb,nd1,nd2,nd3,fert,psid,impf
105   format(i4,3i1,58x,i2,4x,i2,8x,i3)

      write(*,117)char(13),i
117   format(a,i4)

      numdiv(i)=numb*1000+nd1*100+nd2*10+nd3

      if(nd3.gt.0) then
          area(i)=barea/64.
        elseif(nd2.gt.0) then
          area(i)=barea/16.
        elseif(nd1.gt.0) then
          area(i)=barea/4.
        else
          area(i)=barea
      endif

c Skip a line if fertilizer level = 4 (User Defined)

      if(fert.eq.4) read(inp,*)

c Skip line for impoundments if necessary.

      if (impf.gt.0) then
        read(inp,*)
      endif

c Skip lines for point sources if necessary.  For more explanation of the
c code below, see the subroutine "do_pntsrc".

      if (psid.gt.0) then
        do 30 j=1, psid
           call set_pntsrc(pointfmt,inp,buff)
           read(inp,'(A)') buff

c if old format then set PSCODE=2 for feedlots else read type from line

           if (pointfmt.eq.1) then
              pscode=2
           else
              read(buff,'(i1)') pscode
           endif

c skip feedlot info.

           if (pscode.eq.2) then
             read(inp,*)
             read(inp,*)
             read(inp,*)
             read(inp,*)
           endif

30      continue
      endif

      i=i+1
      goto 10

999   ncls=i-1
      rewind 2
      read(inp,'(/)')

      return

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                          CC
CC                      Subroutine Varinit                                  CC
CC                                                                          CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

* this subroutine initializes various variables

      subroutine varinit(ge,awe,chwi,qs1,ti,vi,nlc,psnum,ncls)

	include 'common.blk'
	save /blk/,/psourc/

C-------------------------
C  Parameter description:
C-------------------------
c  The variables have the same discription as the main program's dictionary
c  of variables & definitions.  The variables designated as input below are
c  use but not modified and the variables designated as output are modified
c  it this subroutine.
c
      integer*4 bncls,nlc,psnum,ncls

      real awe(6),barea,chwi,ge(6),qs1,ti,vi

c AWE      output
c CHWI     output
c GE       output
C NCLS     input
c NLC      output
c PSNUM    output
c QS1      output
c TI       output
c VI       output

C-----------------
C local variables:
C-----------------
      integer*4 i,j

      ge(6)=0.
      awe(6)=0.
      chwi=0.
      qs1=0.
      ti=0.
      vi=0.
      nlc=0

      psnum=1
  
      do 44 i=1,maxps
       ptsnum(i)=0
44    continue
  
      do 45 i=1,5
       ge(i)=0.
       awe(i)=0.
45    continue
  
      do 50 i=1,ncls
       do 55 j=1,5
        sme(i,j)=0.0
        sedy(i,j)=0.0
55     continue
       df(i)=0.
       uf(i)=0.
       dist1(i)=0.
       dist2(i)=0.
C -------------- PCELL: SET ALL CELLS AS BEING PRIMARY, NO CELLS DRAIN IN
       pcell(i)=0
       ero(i)=0.
       smro1(i)=0.
       n2(i)=0
       adrain(i)=0
       ant(i)=area(i)*10.
       smero(i)=0.
50    continue
      pcell(ncls+1)=0

      end  


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC             READHEADER                                           CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine readheader(inp,wtshd,barea,bncls,rain,ei,ncdiv,
     *                      descri,mxcl)

c parameters:
C------------
C  wtshd  : output, name of watershed
C  inp    : input,  input file unit number
C  mxcl   : input,  max. number of cells
C  barea  : output, 
C  bncls  : output,
C  rain   : output,
C  ei     : output,
c  ncdiv  : output,
c  descri : output, description of watershed data

      character*30 wtshd
      character*50 descri
      real         barea, rain, ei
      integer*4    bncls, ncdiv, mxcl

c local variables:
C-----------------
      integer*4      ierr
      save /blk/,/psourc/

      read(inp,'(a)',iostat=ierr) wtshd
      if(ierr.ne.0) then
        goto 9999
      endif
      
      read(inp,40,iostat=ierr) barea,bncls,rain,ei,ncdiv,descri
40    format(f4.1,i4,2f6.1,4X,i2,a)
      if(ierr.ne.0) then
        goto 9999
      endif

      if (ncdiv.eq.0) then
         write(*,*) 'Error - Input file not in division format !'
         goto 9999
      end if

      if (bncls.gt.mxcl) then
        goto 9999
      endif
      return

9999  stop
      end

