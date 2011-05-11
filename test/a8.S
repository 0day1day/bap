
.globl	_test
_test:
		movl $0x10, %eax	
		subb $0x10, %al
		movl $0x0, %eax	
		jz out
		dec %eax
out:
		inc %eax
		ret

