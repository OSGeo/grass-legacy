
# digitizing programs and stuff

toxy: toxy.o pfopen.o
	$(CC) $(LDFLAGS) -o toxy toxy.o pfopen.o $(MLIB)/libmap.a $(MLIB)/libgen.a -lm

digin: digin.o digopen.o
	$(CC) $(LDFLAGS) -o digin digin.o digopen.o

#rdccl: mrdccl.o rdccl.o getfor.o getput.o 
#	$(CC) $(LDFLAGS) -o rdccl mrdccl.o rdccl.o getfor.o getput.o 

red1: pred1.o red1.o
	$(CC) $(LDFLAGS) -o red1 pred1.o red1.o

red2: pred2.o red2.o
	$(CC) $(LDFLAGS) -o red2 pred2.o red2.o
