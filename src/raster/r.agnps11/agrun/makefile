#THIS IS THE MAKE DESCRIPTION FILE FOR MODULAR AGNPS

OBJ = agrun.o sub2.o sub1.o loop1.o loop2.o loop3.o outsubs.o iosubs.o
agrun: $(OBJ)
	f77 $(OBJ) -lm -o agrun
agrun.o:    agrun.f common.blk   
	    f77 -c -g agrun.f
sub2.o:     sub2.f  common.blk  
	    f77 -c -g sub2.f
sub1.o:     sub1.f  common.blk  
	    f77 -c -g sub1.f
loop1.o:    loop1.f  common.blk  
	    f77 -c -g loop1.f
loop2.o:    loop2.f  common.blk  
	    f77 -c -g loop2.f
loop3.o:    loop3.f  common.blk  
	    f77 -c -g loop3.f
outsubs.o:  outsubs.f  common.blk  
	    f77 -c -g outsubs.f
iosubs.o:   iosubs.f  common.blk  
	    f77 -c -g iosubs.f
