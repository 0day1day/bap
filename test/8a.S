.globl	_test
_test:
		push %ebp
		mov  %esp,%ebp
		movb -0x1(%ebp), %al
		leave
		ret

