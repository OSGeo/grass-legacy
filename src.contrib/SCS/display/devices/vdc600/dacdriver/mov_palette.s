/ Function: mov_palette
/
/ This procedure moves the RGB intesnities to the DAC registers.  The
/ intensities were stored in the external arrays dac_red, dac_grn and
/ dac_blu when the color wave palette was selected.  The palette is
/ shifted during three vertical retrace periods to avoid flickering.
/
/ Programmed by Paul W. Carlson,  Jan. 1990
/
.text;	.align 4
	.def mov_palette; .val mov_palette; .scl 2; .type 044; .endef; 
.globl mov_palette

mov_palette:
	pushl	%eax			/ save registers
	pushl	%ecx
	pushl	%edx

	movl	$0, %ecx		/ start with 1st element

/ ----- wait for vertical retrace ------------------------
	movw	$0x03DA, %dx
wait1:
	inb	(%dx)
	testb	$8, %al
	jz	wait1
wait2:
	inb	(%dx)
	testb	$8, %al
	jnz	wait2

/ ----- write the first 73 RGB intensities to DAC registers -------
	movw	$0x03C8, %dx
out_loop1:
	movb	%cl, %al		/ selcect DAC register
	outb	(%dx)
	incw	%dx
	movb	dac_red(%ecx), %al	/ write red intensity
	outb	(%dx)
	movb	dac_grn(%ecx), %al	/ write grn intensity
	outb	(%dx)
	movb	dac_blu(%ecx), %al	/ write blu intensity
	outb	(%dx)
	decw	%dx
	incl	%ecx
	cmpl	$73, %ecx
	jne	out_loop1

/ ----- wait for start of vertical retrace -----------------------
	movw	$0x03DA, %dx
wait3:
	inb	(%dx)
	testb	$8, %al
	jz	wait3
wait4:
	inb	(%dx)
	testb	$8, %al
	jnz	wait4

/ ----- write the next 72 RGB intensities to DAC registers -------
	movw	$0x03C8, %dx
out_loop2:
	movb	%cl, %al		/ select DAC register
	outb	(%dx)
	incw	%dx
	movb	dac_red(%ecx), %al	/ write red intensity
	outb	(%dx)
	movb	dac_grn(%ecx), %al	/ write grn intensity
	outb	(%dx)
	movb	dac_blu(%ecx), %al	/ write blu intensity
	outb	(%dx)
	decw	%dx
	incl	%ecx
	cmpl	$145, %ecx
	jne	out_loop2

/ ----- wait for start of vertical retrace -----------------------
	movw	$0x03DA, %dx
wait5:
	inb	(%dx)
	testb	$8, %al
	jz	wait5
wait6:
	inb	(%dx)
	testb	$8, %al
	jnz	wait6

/ ----- write the next 72 RGB intensities to DAC registers -------
	movw	$0x03C8, %dx
out_loop3:
	movb	%cl, %al		/ select DAC register
	outb	(%dx)
	incw	%dx
	movb	dac_red(%ecx), %al	/ write red intensity
	outb	(%dx)
	movb	dac_grn(%ecx), %al	/ write grn intensity
	outb	(%dx)
	movb	dac_blu(%ecx), %al	/ write blu intensity
	outb	(%dx)
	decw	%dx
	incl	%ecx
	cmpl	$217, %ecx
	jne	out_loop3

	popl	%edx			/ resore registers
	popl	%ecx
	popl	%eax
	ret
.def mov_palette; .val .; .scl -1; .endef
