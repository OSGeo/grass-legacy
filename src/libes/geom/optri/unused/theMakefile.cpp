# 1 "theMakefile"

FORMS = /nfs/dogfight.ncsa.uiuc.edu/usr2/public/forms
SOURCE = .
ERNSTSSTUFF = ernst























MACHINE_TYPE = dec

RMEXECUTABLE = rm minmaxer;


























INCLUDE = -I$(SOURCE) -I$(ERNSTSSTUFF)/include
DEBUG = -g
CFLAGS = -traditional -signed 
LIB = -l_sos.$(MACHINE_TYPE) -l_lia.$(MACHINE_TYPE) -l_basic.$(MACHINE_TYPE) -lm
LIBDIR = -L$(ERNSTSSTUFF)/lib




OBJECT = o

CC = cc $(CFLAGS) $(DEBUG)  $(INCLUDE) -c -o $@ $<

.SUFFIXES: .$(OBJECT) $(SUFFIXES)
.c.$(OBJECT): 
	$(CC)



TRIANGULATIONMINIMUMOBJ = novisual.o queue.o heap.o randtree.o stack.o timer.o bitvector.o pqe2qe.o graph.o quadedge.o triangulation.o site.o

ALLTRIOBJ = sos.regular.o planesweep.o flips.o delaunay.o persquadedge.o hdag.o regular.o edgeinsert.o heur.angle.o angle.o heur.slope.o slope.o heur.height.o height.o

OPTRIOBJ = $(ALLTRIOBJ) $(TRIANGULATIONMINIMUMOBJ)

optri: $(OPTRIOBJ) theMakefile
	ar ruv lib_optri_$(MACHINE_TYPE).a $(OPTRIOBJ); ranlib lib_optri_$(MACHINE_TYPE).a; 

Makedepend: 
	mkdepend -c "$(CC) -M" -i $@ $(MINMAXERSOURCE)




