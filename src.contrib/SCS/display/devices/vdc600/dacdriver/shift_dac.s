/ Function: shift_dac
/
/ This procedure shifts the color table arrays.  The red, grn and blu
/ intensities were stored in the external arrays dac_red, dac_grn and
/ dac_blu when the color wave palette was selected.
/
/ Programmed by Paul W. Carlson,  Jan. 1990
/
.text;	.align 4
	.def shift_dac; .val shift_dac; .scl 2; .type 044; .endef; 
.globl shift_dac

shift_dac:
	pushl	%eax			/ save registers
	pushl	%ecx
	pushl	%edx

/       shift the red, grn and blu intensity arrays
/	===========================================
	movl	$215, %ecx		/ save 215th elements
	movb	dac_red(%ecx), %dh
	movb	dac_grn(%ecx), %dl
	movb	dac_blu(%ecx), %ah
	decl	%ecx			/ start with element 214
shift_loop:
	movb	dac_red(%ecx), %al	/ move red(n) into red(n+1)
	movb	%al, dac_red+1(%ecx)
	movb	dac_grn(%ecx), %al	/ move grn(n) into grn(n+1)
	movb	%al, dac_grn+1(%ecx)
	movb	dac_blu(%ecx), %al	/ move blu(n) into blu(n+1)
	movb	%al, dac_blu+1(%ecx)
	decl	%ecx			/ drop down one element
	orl	%ecx, %ecx		/ test if done
	jnz	shift_loop		/ loop if not done
	movb	%dh, dac_red+1		/ get the 215th elements and
	movb	%dl, dac_grn+1		/   put the in element 1
	movb	%ah, dac_blu+1

	popl	%edx			/ resore registers
	popl	%ecx
	popl	%eax
	ret
.def shift_dac; .val .; .scl -1; .endef
