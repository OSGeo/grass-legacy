DIGITALL= \
	check_alloc.o\
	change_to_blank.o\
	getrecord.o\
	digit_edge.o \
	dig_head.o \
	make_digit.o \
	prune.o \
	r_head_rec.o

READALL = \
	check_alloc.o\
	chng2blnk.o\
	getrecord.o\
	r_area_rec.o\
	r_edge_rec.o\
	r_face_rec.o\
	r_feat_rec.o\
	r_head_rec.o\
	r_line_rec.o\
	r_node_rec.o\
	r_point_rec.o\
	read_edge.o\
	read_face.o\
	read_fcin.o\
	read_node.o\
	read_dai.o

all: read read_dai make_digit

read: read.o getrecord.o change_to_blank.o
	$(CC) -o $@ read.o getrecord.o change_to_blank.o

read_dai: $(READALL)
	$(CC) -o $@ $(READALL)

make_digit: $(DIGITALL)
	$(CC) -o $@ $(DIGITALL) coorcnv/libcoorcnv.a -lm

dig_head.o:	header.h
r_head_rec.o:	header.h
digit_edge.o:	header.h
