      integer*2 dbytes,nl,ns,m,ii
      integer*2 input_type, output_type

      integer*2 i(20000)
      real r(20000)
      double precision d(20000)
      equivalence(i(1),r(1),d(1))

      CHARACTER*160 tfile,t1,t2,t3
      read(*,'(a)') tfile
      open(11,file=tfile,status='old')
      read(11,*)input_type,output_type
      read(11,'(a)') t1
      read(11,'(a)') t2
      read(11,'(a)') t3
      open(9,file=t1,status='unknown')
      read(9,*)nl,ns

c input_type 0=integer*2
c input_type 1=real
c input_type 2=double precision

      dbytes=2
      if(input_type.eq.1)then
         dbytes=4
      else if(input_type.eq.2)then
         dbytes=8
      else if(input_type.ne.0)then
         stop
      endif

c output_type 0=integer
c output_type 1=float
c output_type 2=double precision

      if(output_type.ne.0 .and. 
     +   output_type.ne.1 .and.
     +   output_type.ne.2 )stop
 
c direct access binary input file
      open(8,file=t2,status='unknown',form='unformatted',
     +   recl=NS*dbytes,access='direct')

c file for formatted output
      open(10,file=t3,status='unknown')

c output should not promote input integer or real to double precision 
      do 30 m = 1,nl
      if(input_type.eq.0)then
         read(8,rec=m)(i(ii),ii=1,ns)
         if(output_type.eq.0)then
            write(10,*)(i(ii),ii=1,ns)
         else 
            write(10,*)(real(i(ii)),ii=1,ns)
         endif
      else if(input_type.eq.1)then
         read(8,rec=m)(r(ii),ii=1,ns)
         if(output_type.eq.0)then
            write(10,*)(nint(r(ii)),ii=1,ns)
         else 
            write(10,*)(r(ii),ii=1,ns)
         endif
      else 
         read(8,rec=m)(d(ii),ii=1,ns)
         if(output_type.eq.0)then
            write(10,*)(idnint(d(ii)),ii=1,ns)
         else if(output_type.eq.1)then
            write(10,*)(real(d(ii)),ii=1,ns)
         else 
            write(10,*)(d(ii),ii=1,ns)
         endif
      endif
 30   continue

      stop
      end
