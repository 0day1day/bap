
.globl	_test
_test:
		movl $0x10, %eax	
		add  $0xF10, %eax
		sub  $0x01, %eax	
		movl $0x0, %eax	
		jz out
		dec %eax
out:
		inc %eax
		ret

