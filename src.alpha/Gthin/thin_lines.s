	.data
	.lcomm	_n_rows,4
	.lcomm	_n_cols,4
	.lcomm	_pad_size,4
	.lcomm	_box_right,4
	.lcomm	_box_left,4
	.lcomm	_box_top,4
	.lcomm	_box_bottom,4
	.text
LL0:
|#PROC# 04
	.data1
L93:
	.ascii	"Distance transform and skeletonization completed successfully\12\0"
	.data1
L96:
	.ascii	"Linking completed successfully\12\0"
	.data1
L99:
	.ascii	"Thinning completed successfully\12\0"
	LF87	=	0
	LS87	=	0
	LFF87	=	0
	LSS87	=	0
	LP87	=	20
	.data
	.text
	.globl	_thin_lines
_thin_lines:
|#PROLOGUE# 0
	link	a6,#0
|#PROLOGUE# 1
	pea	_pad_size
	pea	_n_cols
	pea	_n_rows
	jbsr	_map_size
	lea	sp@(12),sp
	jbsr	_distance
	tstl	d0
	jne	LE87
	pea	L93
	pea	__iob+20
	jbsr	_fprintf
	addqw	#8,sp
	jbsr	_link
	tstl	d0
	jne	LE87
	pea	L96
	pea	__iob+20
	jbsr	_fprintf
	addqw	#8,sp
	jbsr	_thin
	tstl	d0
	jne	LE87
	pea	L99
	pea	__iob+20
	jbsr	_fprintf
	addqw	#8,sp
LE87:
	unlk	a6
	rts
|#PROC# 04
	.data1
L117:
	.ascii	"%s\72  distance\72  could not find bounding box for lines\12\0"
	.data1
L118:
	.ascii	"Bounding box\72  l = %d, r = %d, t = %d, b = %d\12\0"
	LF100	=	60
	LS100	=	0
	LFF100	=	60
	LSS100	=	0
	LP100	=	32
	.data
	.text
_distance:
|#PROLOGUE# 0
	link	a6,#-60
|#PROLOGUE# 1
	clrl	_box_bottom
	clrl	_box_right
	movl	_n_cols,_box_left
	movl	_n_rows,_box_top
	movl	_pad_size,d0
	subql	#1,d0
	movl	d0,sp@-
	jbsr	_get_a_row
	addqw	#4,sp
	movl	d0,a6@(-28)
	movl	_pad_size,a6@(-4)
	jra	LY00000
LY00001:
	movl	a6@(-28),a6@(-16)
	movl	a6@(-4),sp@-
	jbsr	_get_a_row
	addqw	#4,sp
	movl	d0,a6@(-28)
	movl	d0,a0
	movl	_pad_size,d0
	movb	a0@(-1,d0:l),a6@(-9)
	movl	_pad_size,a6@(-8)
	jra	LY00004
LY00005:
	movl	a6@(-28),a0
	addl	a6@(-8),a0
	tstb	a0@
	jeq	L109
	movl	a6@(-8),d0
	cmpl	_box_left,d0
	jge	L110
	movl	d0,_box_left
L110:
	movl	a6@(-8),d0
	cmpl	_box_right,d0
	jle	L111
	movl	d0,_box_right
L111:
	movl	a6@(-4),d0
	cmpl	_box_top,d0
	jge	L112
	movl	d0,_box_top
L112:
	movl	a6@(-4),d0
	cmpl	_box_bottom,d0
	jle	L113
	movl	d0,_box_bottom
L113:
	moveq	#0,d0
	movb	a6@(-9),d0
	movl	d0,sp@-
	movl	a6@(-16),a0
	addl	a6@(-8),a0
	moveq	#0,d0
	movb	a0@,d0
	movl	d0,sp@-
	jbsr	_min
	addqw	#8,sp
	addqb	#1,d0
	movl	a6@(-28),a0
	addl	a6@(-8),a0
	movb	d0,a0@
L109:
	movl	a6@(-28),a0
	addl	a6@(-8),a0
	movb	a0@,a6@(-9)
	addql	#1,a6@(-8)
LY00004:
	movl	_n_cols,d0
	subl	_pad_size,d0
	cmpl	a6@(-8),d0
	jgt	LY00005
	movl	a6@(-28),sp@-
	movl	a6@(-4),sp@-
	jbsr	_put_a_row
	addqw	#8,sp
	addql	#1,a6@(-4)
LY00000:
	movl	_n_rows,d0
	subl	_pad_size,d0
	cmpl	a6@(-4),d0
	jgt	LY00001
	movl	_box_right,d0
	cmpl	_box_left,d0
	jlt	L2000000
	movl	_box_bottom,d0
	cmpl	_box_top,d0
	jge	L116
L2000000:
	movl	_error_prefix,sp@-
	pea	L117
	pea	__iob+40
	jbsr	_fprintf
	lea	sp@(12),sp
	moveq	#-1,d0
	jra	LE100
L116:
	movl	_box_bottom,sp@-
	movl	_box_top,sp@-
	movl	_box_right,sp@-
	movl	_box_left,sp@-
	pea	L118
	pea	__iob+20
	jbsr	_fprintf
	lea	sp@(24),sp
	moveq	#2,d1
	movl	d1,a6@(-32)
	moveq	#4,d1
	movl	d1,a6@(-36)
	movl	_n_cols,sp@-
	jbsr	_G_malloc
	addqw	#4,sp
	movl	d0,a6@(-52)
	movl	_n_cols,sp@-
	jbsr	_G_malloc
	addqw	#4,sp
	movl	d0,a6@(-48)
	movl	_n_cols,sp@-
	jbsr	_G_malloc
	addqw	#4,sp
	movl	d0,a6@(-44)
	movl	_box_bottom,a6@(-4)
	jra	LY00002
LY00003:
	movl	a6@(-4),sp@-
	jbsr	_get_a_row
	addqw	#4,sp
	movl	d0,a6@(-16)
	movl	a6@(-4),d0
	addql	#1,d0
	movl	d0,sp@-
	jbsr	_get_a_row
	addqw	#4,sp
	movl	d0,a6@(-24)
	movl	a6@(-4),d0
	addql	#2,d0
	movl	d0,sp@-
	jbsr	_get_a_row
	addqw	#4,sp
	movl	d0,a6@(-20)
	movl	a6@(-4),d0
	addql	#4,d0
	movl	d0,sp@-
	jbsr	_get_a_row
	addqw	#4,sp
	movl	d0,a6@(-28)
	movl	a6@(-16),a0
	movl	_box_right,d0
	movb	a0@(1,d0:l),a6@(-9)
	clrl	a6@(-56)
	jra	LY00006
LY00007:
	movl	a6@(-52),a0
	addl	a6@(-56),a0
	clrb	a0@
	addql	#1,a6@(-56)
LY00006:
	movl	a6@(-56),d0
	cmpl	_n_cols,d0
	jlt	LY00007
	movl	_box_right,a6@(-8)
	jra	LY00008
LY00009:
	movl	a6@(-24),a0
	addl	a6@(-8),a0
	moveq	#0,d0
	movb	a0@,d0
	movl	d0,sp@-
	moveq	#0,d0
	movb	a6@(-9),d0
	movl	d0,sp@-
	jbsr	_min
	addqw	#8,sp
	addql	#1,d0
	movl	d0,sp@-
	movl	a6@(-16),a0
	addl	a6@(-8),a0
	moveq	#0,d0
	movb	a0@,d0
	movl	d0,sp@-
	jbsr	_min
	addqw	#8,sp
	movl	a6@(-16),a0
	addl	a6@(-8),a0
	movb	d0,a0@
	movb	d0,a6@(-9)
	tstl	a6@(-32)
	jne	L128
	movl	a6@(-20),a0
	addl	a6@(-8),a0
	tstb	a0@
	jeq	L125
	movl	a6@(-20),a0
	movl	a6@(-8),d0
	movb	a0@(-2,d0:l),d0
	andl	#255,d0
	movl	a6@(-8),d1
	movb	a0@(2,d1:l),d1
	andl	#255,d1
	addl	d1,d0
	movl	a6@(-16),a0
	addl	a6@(-8),a0
	moveq	#0,d1
	movb	a0@,d1
	addl	d1,d0
	movl	a6@(-28),a0
	addl	a6@(-8),a0
	moveq	#0,d1
	movb	a0@,d1
	addl	d1,d0
	movl	a6@(-20),a0
	addl	a6@(-8),a0
	moveq	#0,d1
	movb	a0@,d1
	asll	#2,d1
	subl	d1,d0
	movl	d0,a6@(-60)
	cmpl	#-2,a6@(-60)
	jgt	L125
	movl	a6@(-52),a0
	addl	a6@(-8),a0
	movl	a6@(-20),a1
	addl	a6@(-8),a1
	movb	a1@,a0@
	jra	L125
L128:
	subql	#1,a6@(-32)
L125:
	subql	#1,a6@(-8)
LY00008:
	movl	a6@(-8),d0
	cmpl	_box_left,d0
	jge	LY00009
	movl	a6@(-16),sp@-
	movl	a6@(-4),sp@-
	jbsr	_put_a_row
	addqw	#8,sp
	tstl	a6@(-36)
	jne	L132
	movl	a6@(-44),sp@-
	movl	a6@(-4),d0
	addql	#4,d0
	movl	d0,sp@-
	jbsr	_put_a_row
	addqw	#8,sp
	jra	L133
L132:
	subql	#1,a6@(-36)
L133:
	movl	a6@(-44),a6@(-40)
	movl	a6@(-48),a6@(-44)
	movl	a6@(-52),a6@(-48)
	movl	a6@(-40),a6@(-52)
	subql	#1,a6@(-4)
LY00002:
	movl	_box_top,d0
	subql	#2,d0
	cmpl	a6@(-4),d0
	jle	LY00003
	movl	a6@(-52),sp@-
	jbsr	_free
	addqw	#4,sp
	movl	a6@(-48),sp@-
	jbsr	_free
	addqw	#4,sp
	movl	a6@(-44),sp@-
	jbsr	_free
	addqw	#4,sp
	moveq	#0,d0
LE100:
	unlk	a6
	rts
|#PROC# 04
	.data1
L156:
	.ascii	"South (%d,%d)\12\0"
	.data1
L158:
	.ascii	"SouthEast (%d,%d)\12\0"
	.data1
L160:
	.ascii	"East (%d,%d)\12\0"
	.data1
L162:
	.ascii	"NorthEast (%d,%d)\12\0"
	.data1
L164:
	.ascii	"North (%d,%d)\12\0"
	LF135	=	36
	LS135	=	0
	LFF135	=	36
	LSS135	=	0
	LP135	=	24
	.data
	.text
_link:
|#PROLOGUE# 0
	link	a6,#-36
|#PROLOGUE# 1
	movl	_box_top,d0
	addql	#3,d0
	movl	d0,a6@(-4)
	jra	LY00010
LY00011:
	movl	a6@(-4),d0
	subql	#3,d0
	movl	d0,sp@-
	jbsr	_get_a_row
	addqw	#4,sp
	movl	d0,a6@(-36)
	movl	a6@(-4),d0
	subql	#2,d0
	movl	d0,sp@-
	jbsr	_get_a_row
	addqw	#4,sp
	movl	d0,a6@(-32)
	movl	a6@(-4),d0
	subql	#1,d0
	movl	d0,sp@-
	jbsr	_get_a_row
	addqw	#4,sp
	movl	d0,a6@(-28)
	movl	a6@(-4),sp@-
	jbsr	_get_a_row
	addqw	#4,sp
	movl	d0,a6@(-24)
	movl	a6@(-4),d0
	addql	#1,d0
	movl	d0,sp@-
	jbsr	_get_a_row
	addqw	#4,sp
	movl	d0,a6@(-20)
	movl	a6@(-4),d0
	addql	#2,d0
	movl	d0,sp@-
	jbsr	_get_a_row
	addqw	#4,sp
	movl	d0,a6@(-16)
	movl	a6@(-4),d0
	addql	#3,d0
	movl	d0,sp@-
	jbsr	_get_a_row
	addqw	#4,sp
	movl	d0,a6@(-12)
	movl	_box_left,a6@(-8)
	jra	LY00016
LY00017:
	movl	a6@(-24),a0
	addl	a6@(-8),a0
	tstb	a0@
	jeq	L140
	movl	a6@(-20),sp@-
	movl	a6@(-24),sp@-
	movl	a6@(-28),sp@-
	movl	a6@(-8),sp@-
	jbsr	_linked
	lea	sp@(16),sp
	tstl	d0
	jne	L140
	pea	a6@(-36)
	movl	a6@(-8),sp@-
	jbsr	_North
	addqw	#8,sp
	tstl	d0
	jne	L147
	pea	a6@(-36)
	movl	a6@(-8),sp@-
	jbsr	_NEast
	addqw	#8,sp
	tstl	d0
	jne	L149
	pea	a6@(-36)
	movl	a6@(-8),sp@-
	jbsr	_East
	addqw	#8,sp
	tstl	d0
	jne	L151
	pea	a6@(-36)
	movl	a6@(-8),sp@-
	jbsr	_SEast
	addqw	#8,sp
	tstl	d0
	jne	L153
	pea	a6@(-36)
	movl	a6@(-8),sp@-
	jbsr	_South
	addqw	#8,sp
	tstl	d0
	jeq	L140
	movl	a6@(-8),sp@-
	movl	a6@(-4),sp@-
	pea	L156
	jra	LY00012
L153:
	movl	a6@(-8),sp@-
	movl	a6@(-4),sp@-
	pea	L158
	jra	LY00012
L151:
	movl	a6@(-8),sp@-
	movl	a6@(-4),sp@-
	pea	L160
	jra	LY00012
L149:
	movl	a6@(-8),sp@-
	movl	a6@(-4),sp@-
	pea	L162
	jra	LY00012
L147:
	movl	a6@(-8),sp@-
	movl	a6@(-4),sp@-
	pea	L164
LY00012:
	pea	__iob+20
	jbsr	_fprintf
	lea	sp@(16),sp
L140:
	addql	#1,a6@(-8)
LY00016:
	movl	_box_right,d0
	subql	#3,d0
	cmpl	a6@(-8),d0
	jge	LY00017
	movl	a6@(-36),sp@-
	movl	a6@(-4),d0
	subql	#3,d0
	movl	d0,sp@-
	jbsr	_put_a_row
	addqw	#8,sp
	movl	a6@(-32),sp@-
	movl	a6@(-4),d0
	subql	#2,d0
	movl	d0,sp@-
	jbsr	_put_a_row
	addqw	#8,sp
	movl	a6@(-28),sp@-
	movl	a6@(-4),d0
	subql	#1,d0
	movl	d0,sp@-
	jbsr	_put_a_row
	addqw	#8,sp
	movl	a6@(-24),sp@-
	movl	a6@(-4),sp@-
	jbsr	_put_a_row
	addqw	#8,sp
	movl	a6@(-20),sp@-
	movl	a6@(-4),d0
	addql	#1,d0
	movl	d0,sp@-
	jbsr	_put_a_row
	addqw	#8,sp
	movl	a6@(-16),sp@-
	movl	a6@(-4),d0
	addql	#2,d0
	movl	d0,sp@-
	jbsr	_put_a_row
	addqw	#8,sp
	movl	a6@(-12),sp@-
	movl	a6@(-4),d0
	addql	#3,d0
	movl	d0,sp@-
	jbsr	_put_a_row
	addqw	#8,sp
	addql	#1,a6@(-4)
LY00010:
	movl	_box_bottom,d0
	subql	#3,d0
	cmpl	a6@(-4),d0
	jge	LY00011
	moveq	#0,d0
	unlk	a6
	rts
|#PROC# 04
	LF165	=	8
	LS165	=	0
	LFF165	=	8
	LSS165	=	0
	LP165	=	8
	.data
	.text
_linked:
|#PROLOGUE# 0
	link	a6,#-8
|#PROLOGUE# 1
	movl	a6@(8),d0
	addql	#1,d0
	movl	d0,a6@(-8)
	movl	a6@(8),d0
	subql	#1,d0
	movl	d0,a6@(-4)
	movl	a6@(16),a0
	addl	d0,a0
	tstb	a0@
	jeq	L2000002
	movl	a6@(16),a0
	addl	a6@(-8),a0
	tstb	a0@
	jne	L2000001
L2000002:
	movl	a6@(12),a0
	addl	a6@(8),a0
	tstb	a0@
	jeq	L167
	movl	a6@(20),a0
	addl	a6@(8),a0
	tstb	a0@
	jeq	L167
L2000001:
	moveq	#1,d0
	jra	LE165
L167:
	movl	a6@(12),a0
	addl	a6@(-8),a0
	tstb	a0@
	jeq	L2000004
	movl	a6@(20),a0
	addl	a6@(-4),a0
	tstb	a0@
	jne	L2000003
L2000004:
	movl	a6@(12),a0
	addl	a6@(-4),a0
	tstb	a0@
	jeq	L168
	movl	a6@(20),a0
	addl	a6@(-8),a0
	tstb	a0@
	jeq	L168
L2000003:
	moveq	#1,d0
	jra	LE165
L168:
	movl	a6@(12),a0
	addl	a6@(-8),a0
	tstb	a0@
	jne	L2000005
	movl	a6@(20),a0
	addl	a6@(-4),a0
	tstb	a0@
	jeq	L169
L2000005:
	movl	a6@(12),a0
	addl	a6@(-4),a0
	tstb	a0@
	jne	L2000006
	movl	a6@(20),a0
	addl	a6@(-8),a0
	tstb	a0@
	jeq	L169
L2000006:
	moveq	#1,d0
	jra	LE165
L169:
	moveq	#0,d0
LE165:
	unlk	a6
	rts
|#PROC# 04
	LF170	=	0
	LS170	=	0
	LFF170	=	0
	LSS170	=	0
	LP170	=	8
	.data
	.text
_North:
|#PROLOGUE# 0
	link	a6,#0
|#PROLOGUE# 1
	movl	a6@(12),a0
	movl	a0@(8),a0
	addl	a6@(8),a0
	tstb	a0@
	jne	L172
	movl	a6@(12),a0
	movl	a0@(4),a0
	addl	a6@(8),a0
	tstb	a0@
	jne	L2000007
	movl	a6@(12),a0
	movl	a0@,a0
	addl	a6@(8),a0
	tstb	a0@
	jeq	L172
L2000007:
	movl	a6@(12),a0
	movl	a0@(8),a0
	addl	a6@(8),a0
	movl	a6@(12),a1
	movl	a1@(4),a1
	addl	a6@(8),a1
	movb	#12,a1@
	movb	a1@,a0@
	moveq	#1,d0
	jra	LE170
L172:
	moveq	#0,d0
LE170:
	unlk	a6
	rts
|#PROC# 04
	LF174	=	0
	LS174	=	0
	LFF174	=	0
	LSS174	=	0
	LP174	=	8
	.data
	.text
_NEast:
|#PROLOGUE# 0
	link	a6,#0
|#PROLOGUE# 1
	movl	a6@(12),a0
	movl	a0@(8),a0
	addl	a6@(8),a0
	tstb	a0@
	jne	L176
	movl	a6@(12),a0
	movl	a0@(8),a0
	movl	a6@(8),d0
	tstb	a0@(1,d0:l)
	jne	L176
	movl	a6@(12),a0
	movl	a0@(12),a0
	tstb	a0@(1,d0:l)
	jne	L176
	movl	a6@(12),a0
	movl	a0@,a0
	tstb	a0@(1,d0:l)
	jeq	L177
	movl	a6@(12),a0
	movl	a0@(8),a0
	addl	d0,a0
	movl	a6@(12),a1
	movl	a1@(4),a1
	movb	#12,a1@(1,d0:l)
	movb	a1@(1,d0:l),a0@(1)
	moveq	#1,d0
	jra	LE174
L177:
	movl	a6@(12),a0
	movl	a0@,a0
	movl	a6@(8),d0
	tstb	a0@(2,d0:l)
	jne	L2000008
	movl	a6@(12),a0
	movl	a0@,a0
	tstb	a0@(3,d0:l)
	jne	L2000008
	movl	a6@(12),a0
	movl	a0@(4),a0
	tstb	a0@(3,d0:l)
	jeq	L178
L2000008:
	movl	a6@(12),a0
	movl	a0@(8),a0
	addl	a6@(8),a0
	movl	a6@(12),a1
	movl	a1@(4),a1
	movl	a6@(8),d0
	movb	#12,a1@(2,d0:l)
	movb	a1@(2,d0:l),a0@(1)
	moveq	#1,d0
	jra	LE174
L178:
	movl	a6@(12),a0
	movl	a0@(8),a0
	movl	a6@(8),d0
	tstb	a0@(3,d0:l)
	jeq	L176
	movl	a6@(12),a0
	movl	a0@(8),a0
	addl	d0,a0
	movl	a6@(12),a1
	movl	a1@(8),a1
	movb	#12,a1@(2,d0:l)
	movb	a1@(2,d0:l),a0@(1)
	moveq	#1,d0
	jra	LE174
L176:
	moveq	#0,d0
LE174:
	unlk	a6
	rts
|#PROC# 04
	LF180	=	0
	LS180	=	0
	LFF180	=	0
	LSS180	=	0
	LP180	=	8
	.data
	.text
_East:
|#PROLOGUE# 0
	link	a6,#0
|#PROLOGUE# 1
	movl	a6@(12),a0
	movl	a0@(12),a0
	movl	a6@(8),d0
	tstb	a0@(1,d0:l)
	jne	L182
	movl	a6@(12),a0
	movl	a0@(12),a0
	tstb	a0@(2,d0:l)
	jne	L2000009
	movl	a6@(12),a0
	movl	a0@(12),a0
	tstb	a0@(3,d0:l)
	jeq	L182
L2000009:
	movl	a6@(12),a0
	movl	a0@(12),a0
	addl	a6@(8),a0
	movl	a6@(12),a1
	movl	a1@(12),a1
	movl	a6@(8),d0
	movb	#12,a1@(2,d0:l)
	movb	a1@(2,d0:l),a0@(1)
	moveq	#1,d0
	jra	LE180
L182:
	moveq	#0,d0
LE180:
	unlk	a6
	rts
|#PROC# 04
	LF184	=	0
	LS184	=	0
	LFF184	=	0
	LSS184	=	0
	LP184	=	8
	.data
	.text
_SEast:
|#PROLOGUE# 0
	link	a6,#0
|#PROLOGUE# 1
	movl	a6@(12),a0
	movl	a0@(12),a0
	movl	a6@(8),d0
	tstb	a0@(1,d0:l)
	jne	L186
	movl	a6@(12),a0
	movl	a0@(16),a0
	tstb	a0@(1,d0:l)
	jne	L186
	movl	a6@(12),a0
	movl	a0@(16),a0
	addl	d0,a0
	tstb	a0@
	jne	L186
	movl	a6@(12),a0
	movl	a0@(20),a0
	addl	d0,a0
	tstb	a0@
	jne	L186
	movl	a6@(12),a0
	movl	a0@(24),a0
	addl	d0,a0
	tstb	a0@
	jne	L186
	movl	a6@(12),a0
	movl	a0@(16),a0
	tstb	a0@(3,d0:l)
	jne	L2000010
	movl	a6@(12),a0
	movl	a0@(20),a0
	tstb	a0@(3,d0:l)
	jne	L2000010
	movl	a6@(12),a0
	movl	a0@(24),a0
	tstb	a0@(3,d0:l)
	jne	L2000010
	movl	a6@(12),a0
	movl	a0@(24),a0
	tstb	a0@(2,d0:l)
	jne	L2000010
	movl	a6@(12),a0
	movl	a0@(24),a0
	tstb	a0@(1,d0:l)
	jeq	L186
L2000010:
	movl	a6@(12),a0
	movl	a0@(16),a0
	movl	a6@(8),d0
	movb	#12,a0@(1,d0:l)
	moveq	#1,d0
	jra	LE184
L186:
	moveq	#0,d0
LE184:
	unlk	a6
	rts
|#PROC# 04
	LF188	=	0
	LS188	=	0
	LFF188	=	0
	LSS188	=	0
	LP188	=	8
	.data
	.text
_South:
|#PROLOGUE# 0
	link	a6,#0
|#PROLOGUE# 1
	movl	a6@(12),a0
	movl	a0@(16),a0
	movl	a6@(8),d0
	tstb	a0@(1,d0:l)
	jne	L190
	movl	a6@(12),a0
	movl	a0@(16),a0
	addl	d0,a0
	tstb	a0@
	jne	L190
	movl	a6@(12),a0
	movl	a0@(24),a0
	addl	d0,a0
	tstb	a0@
	jeq	L190
	movl	a6@(12),a0
	movl	a0@(16),a0
	addl	d0,a0
	movb	#12,a0@
	moveq	#1,d0
	jra	LE188
L190:
	moveq	#0,d0
LE188:
	unlk	a6
	rts
|#PROC# 04
	LF192	=	56
	LS192	=	4
	LFF192	=	52
	LSS192	=	0
	LP192	=	16
	.data
	.text
_thin:
|#PROLOGUE# 0
	link	a6,#-56
	movl	d2,sp@
|#PROLOGUE# 1
	movl	_box_top,a6@(-32)
	jra	LY00018
LY00019:
	movl	a6@(-32),d0
	subql	#1,d0
	movl	d0,sp@-
	jbsr	_get_a_row
	addqw	#4,sp
	movl	d0,a6@(-40)
	movl	a6@(-32),sp@-
	jbsr	_get_a_row
	addqw	#4,sp
	movl	d0,a6@(-44)
	movl	a6@(-32),d0
	addql	#1,d0
	movl	d0,sp@-
	jbsr	_get_a_row
	addqw	#4,sp
	movl	d0,a6@(-48)
	movl	a6@(-32),d0
	addql	#2,d0
	movl	d0,sp@-
	jbsr	_get_a_row
	addqw	#4,sp
	movl	d0,a6@(-52)
	movl	a6@(-32),d0
	cmpl	_box_top,d0
	jne	L197
	movl	_box_left,a6@(-36)
	jra	LY00020
LY00021:
	movl	a6@(-44),a0
	addl	a6@(-36),a0
	tstb	a0@
	jeq	L198
	movl	a6@(-40),a0
	addl	a6@(-36),a0
	tstb	a0@
	sne	d0
	negb	d0
	movl	a6@(-40),a0
	movl	a6@(-36),d1
	moveq	#0,d2
	tstb	a0@(1,d1:l)
	sne	d2
	negb	d2
	asll	#1,d2
	orb	d2,d0
	movl	a6@(-44),a0
	moveq	#0,d2
	tstb	a0@(1,d1:l)
	sne	d2
	negb	d2
	asll	#2,d2
	orb	d2,d0
	movl	a6@(-52),a0
	moveq	#0,d2
	tstb	a0@(1,d1:l)
	sne	d2
	negb	d2
	asll	#3,d2
	orb	d2,d0
	addl	d1,a0
	moveq	#0,d1
	tstb	a0@
	sne	d1
	negb	d1
	asll	#4,d1
	orb	d1,d0
	movl	a6@(-52),a0
	movl	a6@(-36),d1
	moveq	#0,d2
	tstb	a0@(-1,d1:l)
	sne	d2
	negb	d2
	asll	#5,d2
	orb	d2,d0
	movl	a6@(-44),a0
	moveq	#0,d2
	tstb	a0@(-1,d1:l)
	sne	d2
	negb	d2
	asll	#6,d2
	orb	d2,d0
	movl	a6@(-40),a0
	moveq	#0,d2
	tstb	a0@(-1,d1:l)
	sne	d2
	negb	d2
	asll	#7,d2
	orb	d2,d0
	movl	a6@(-44),a0
	addl	d1,a0
	movb	d0,a0@
L198:
	addql	#1,a6@(-36)
LY00020:
	movl	a6@(-36),d0
	cmpl	_box_right,d0
	jle	LY00021
L197:
	movl	_box_left,d0
	subql	#1,d0
	movl	d0,a6@(-36)
	movl	_box_left,a6@(-4)
	jra	LY00022
LY00023:
	movl	a6@(-4),d0
	cmpl	_box_right,d0
	jgt	L205
	movl	a6@(-48),a0
	addl	d0,a0
	tstb	a0@
	jeq	L205
	movl	a6@(-44),a0
	addl	d0,a0
	tstb	a0@
	sne	d0
	negb	d0
	movl	a6@(-44),a0
	movl	a6@(-4),d1
	moveq	#0,d2
	tstb	a0@(1,d1:l)
	sne	d2
	negb	d2
	asll	#1,d2
	orb	d2,d0
	movl	a6@(-48),a0
	moveq	#0,d2
	tstb	a0@(1,d1:l)
	sne	d2
	negb	d2
	asll	#2,d2
	orb	d2,d0
	movl	a6@(-52),a0
	moveq	#0,d2
	tstb	a0@(1,d1:l)
	sne	d2
	negb	d2
	asll	#3,d2
	orb	d2,d0
	addl	d1,a0
	moveq	#0,d1
	tstb	a0@
	sne	d1
	negb	d1
	asll	#4,d1
	orb	d1,d0
	movl	a6@(-52),a0
	movl	a6@(-4),d1
	moveq	#0,d2
	tstb	a0@(-1,d1:l)
	sne	d2
	negb	d2
	asll	#5,d2
	orb	d2,d0
	movl	a6@(-48),a0
	moveq	#0,d2
	tstb	a0@(-1,d1:l)
	sne	d2
	negb	d2
	asll	#6,d2
	orb	d2,d0
	movl	a6@(-44),a0
	moveq	#0,d2
	tstb	a0@(-1,d1:l)
	sne	d2
	negb	d2
	asll	#7,d2
	orb	d2,d0
	movl	a6@(-48),a0
	addl	d1,a0
	movb	d0,a0@
L205:
	movl	a6@(-36),d0
	cmpl	_box_left,d0
	jlt	L202
	movl	a6@(-44),a0
	addl	d0,a0
	moveq	#0,d0
	movb	a0@,d0
	movl	d0,a6@(-12)
	jeq	L202
	movb	a6@(-9),d0
	moveq	#85,d2
	andl	d2,d0
	asll	#1,d0
	orl	a6@(-12),d0
	movl	d0,a6@(-16)
	asll	#1,d0
	movl	d0,a6@(-20)
	btst	#7,a6@(-13)
	jeq	L209
	orb	#1,a6@(-17)
L209:
	movl	a6@(-16),d0
	notl	d0
	andl	#255,d0
	andl	d0,a6@(-20)
	clrl	a6@(-28)
	clrl	a6@(-24)
	clrl	a6@(-8)
LY00025:
	btst	#0,a6@(-13)
	jeq	L213
	addql	#1,a6@(-28)
L213:
	btst	#0,a6@(-17)
	jeq	L214
	addql	#1,a6@(-24)
L214:
	movl	a6@(-16),d0
	asrl	#1,d0
	movl	d0,a6@(-16)
	movl	a6@(-20),d0
	asrl	#1,d0
	movl	d0,a6@(-20)
	addql	#1,a6@(-8)
	cmpl	#8,a6@(-8)
	jlt	LY00025
	cmpl	#1,a6@(-24)
	jgt	L2000011
	cmpl	#1,a6@(-24)
	jne	L215
	cmpl	#2,a6@(-28)
	jgt	L215
L2000011:
	movl	a6@(-44),a0
	addl	a6@(-36),a0
	movb	#1,a0@
	jra	L202
L215:
	movl	a6@(-44),a0
	addl	a6@(-36),a0
	clrb	a0@
	movl	a6@(-40),a0
	addl	a6@(-36),a0
	andb	#239,a0@
	movl	a6@(-40),a0
	movl	a6@(-36),d0
	andb	#223,a0@(1,d0:l)
	movl	a6@(-44),a0
	movl	a6@(-36),d0
	andb	#191,a0@(1,d0:l)
	movl	a6@(-48),a0
	movl	a6@(-36),d0
	andb	#127,a0@(1,d0:l)
	movl	a6@(-48),a0
	addl	a6@(-36),a0
	andb	#254,a0@
	movl	a6@(-48),a0
	movl	a6@(-36),d0
	andb	#253,a0@(-1,d0:l)
	movl	a6@(-44),a0
	movl	a6@(-36),d0
	andb	#251,a0@(-1,d0:l)
	movl	a6@(-40),a0
	movl	a6@(-36),d0
	andb	#247,a0@(-1,d0:l)
L202:
	addql	#1,a6@(-4)
	addql	#1,a6@(-36)
LY00022:
	movl	a6@(-36),d0
	cmpl	_box_right,d0
	jle	LY00023
	movl	a6@(-44),sp@-
	movl	a6@(-32),sp@-
	jbsr	_put_a_row
	addqw	#8,sp
	movl	a6@(-48),sp@-
	movl	a6@(-32),d0
	addql	#1,d0
	movl	d0,sp@-
	jbsr	_put_a_row
	addqw	#8,sp
	addql	#1,a6@(-32)
LY00018:
	movl	a6@(-32),d0
	cmpl	_box_bottom,d0
	jle	LY00019
	moveq	#0,d0
	movl	a6@(-56),d2
	unlk	a6
	rts
|#PROC# 04
	LF217	=	0
	LS217	=	0
	LFF217	=	0
	LSS217	=	0
	LP217	=	8
	.data
	.text
_min:
|#PROLOGUE# 0
	link	a6,#0
|#PROLOGUE# 1
	movl	a6@(8),d0
	cmpl	a6@(12),d0
	jlt	LE217
	movl	a6@(12),d0
LE217:
	unlk	a6
	rts
|#PROC# 04
	LF221	=	0
	LS221	=	0
	LFF221	=	0
	LSS221	=	0
	LP221	=	8
	.data
	.text
_before:
|#PROLOGUE# 0
	link	a6,#0
|#PROLOGUE# 1
	movl	a6@(8),a0
	movl	a0@(-4),d0
	unlk	a6
	rts
