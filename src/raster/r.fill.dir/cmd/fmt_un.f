
      integer dbytes
      parameter(dbytes=8)

      double precision i(20000)
      character*160 tfile,t1,t2,t3
      integer nl,ns,m,m2

      read(*,'(a)') tfile
      open(11,file=tfile,status='old')
      read(11,'(a)') t1
      read(11,'(a)') t2
      read(11,'(a)') t3
      open(10,file=t1,status='old')
      read(10,*)nl,ns
      open(8,file=t3,status='new',form='unformatted',
     +   recl=ns*dbytes,access='direct')
      open(9,file=t2,status='old')
      do 20 m = 1,nl
      do 21 m2 = 1,ns
 21   read(9,*)i(m2)
 20   write(8,rec=m)(i(m2),m2=1,ns)
      stop
      end
