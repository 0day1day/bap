.globl	_test
_test:
		push %rbp
		mov  %rsp,%rbp
		movb -0x1(%rbp), %al
		leave
		ret

