LL0:
	.seg	"data"
	.seg	"text"
	.proc 04
	.global	_main
_main:
	!#PROLOGUE# 0
	sethi	%hi(LF12),%g1
	add	%g1,%lo(LF12),%g1
	save	%sp,%g1,%sp
	!#PROLOGUE# 1
	.seg	"data1"
L15:
	.ascii	"hello world\012\0"
	.seg	"text"
	set	L15,%o0
	call	_printf,1
	nop
	set	0x30d40,%o0
	call	_malloc,1
	nop
	st	%o0,[%fp+-0x4]
	.seg	"data1"
L17:
	.ascii	"pointer %d\012\0"
	.seg	"text"
	set	L17,%o0
	ld	[%fp+-0x4],%o1
	call	_printf,2
	nop
	st	%g0,[%fp+-0x4]
	.seg	"data1"
L18:
	.ascii	"pointer %d\012\0"
	.seg	"text"
	set	L18,%o0
	ld	[%fp+-0x4],%o1
	call	_printf,2
	nop
LE12:
	ret
	restore
       LF12 = -104
	LP12 = 96
	LST12 = 96
	LT12 = 96
	.seg	"data"
