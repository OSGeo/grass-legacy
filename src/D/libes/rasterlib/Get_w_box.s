LL0:
	.seg	"data"
	.seg	"text"
	.proc 04
	.global	_R_get_location_with_box
_R_get_location_with_box:
	!#PROLOGUE# 0
	sethi	%hi(LF12),%g1
	add	%g1,%lo(LF12),%g1
	save	%sp,%g1,%sp
	!#PROLOGUE# 1
	st	%i0,[%fp+0x44]
	st	%i1,[%fp+0x48]
	st	%i2,[%fp+0x4c]
	st	%i3,[%fp+0x50]
	st	%i4,[%fp+0x54]
	mov	0x8,%o0
	call	__send_ident,1
	nop
	ld	[%fp+0x44],%o0
	st	%o0,[%fp+-0x4]
	sub	%fp,0x4,%o0
	call	__send_int,1
	nop
	ld	[%fp+0x48],%o1
	st	%o1,[%fp+-0x4]
	sub	%fp,0x4,%o0
	call	__send_int,1
	nop
	ld	[%fp+0x4c],%o2
	ld	[%o2],%o3
	st	%o3,[%fp+-0x4]
	sub	%fp,0x4,%o0
	call	__send_int,1
	nop
	ld	[%fp+0x50],%o4
	ld	[%o4],%o5
	st	%o5,[%fp+-0x4]
	sub	%fp,0x4,%o0
	call	__send_int,1
	nop
	ld	[%fp+0x4c],%o0
	call	__get_int,1
	nop
	ld	[%fp+0x50],%o0
	call	__get_int,1
	nop
	ld	[%fp+0x54],%o0
	call	__get_int,1
	nop
	mov	0,%o0
LE12:
	mov	%o0,%i0
	ret
	restore
       LF12 = -104
	LP12 = 96
	LT12 = 96
	.seg	"data"
