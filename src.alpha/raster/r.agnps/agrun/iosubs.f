CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC                                                                  CC
CC                   INFILES                                        CC
CC                                                                  CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  
      subroutine infiles(infile,ofname,inp,np)

      integer position
      integer*4 inp,np
      character*80 infile,ofname


c     write(*,*)' Enter the data file name with path '
      read(*,'(a)') infile
      write(*,*) infile
      open(inp,file=infile,status='old')
      inquire(inp,name=infile)

      ofname=infile
      position=index(ofname,'.')
      ofname(position+1:position+4)='nps'

      open(np,file=ofname,status='unknown')

      end
