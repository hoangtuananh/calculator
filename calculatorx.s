; ********************
; *                  *
; *  ARM Calculator  *
; *                  *
; ********************

; This program implements an operator-precedence parser in ARM assembly language, and
; controls a memory-mapped I/O console to create a spreadsheet-like calculator.

; ***************
; *             *
; *  Constants  *
; *             *
; ***************

; I/O Console memory-mapped I/O locations
console	= 0x10000000
aReg	= 0
bReg	= 4
cReg	= 8
dReg	= 12
eReg	= 16
runFlag = 0x20
tokens	= 0x100

; Stack sizes
operandStackSize  = 64
operatorStackSize = 128

; Halt code for simulator SWI instruction
halt	= 0x11

; *******************************
; *                             *
; *  State and Operator Tables  *
; *                             *
; *******************************

; If memory starts at 0, put a halt there in case a bug causes the program to branch to 0
	.text
	swi	halt

; The state table is a table of branch addresses that tell the state machine what
; to do in each state for each token type
stateTable:
	;	  A State       B State
	;	---------------------------------------------
	.word	pushLiteral , syntaxError	; literal
	.word	pushRegister, syntaxError	; register
	.word	unaryOp     , binaryOp		; operator
	.word	subExpr     , syntaxError	; open paren
	.word	syntaxError , endSub		; close paren
	.word	syntaxError , endExpr		; end
	.word	syntaxError , syntaxError	; error

; The operator table gives the precedence and action subroutine address for each operator
; token when used as a unary or binary operator. When there is no unary or binary
; operation, the precedence and action address are 0. All other precedence values must
; be greater than 0, but otherwise can take on any value. Only relative precedence matters,
; and was chosen to match the relative precedence in Java, but can be changed as desired.
; The values were chosen to be multiples of 10 so that new values can be selected in
; between existing values without having to change the entire table. A more sophisticated
; table would also have left- and right-associative indicators, but here we assume all
; binary operators are left-associative and all unary operators are right-associative.
opTable:
	;	     Unary       |      Binary
	;	---------------------------------------------   
	.word	  0,          0,   70, uShiftRight	; >>>
	.word	  0,          0,   70, shiftLeft	; <<
	.word	  0,          0,   70, shiftRight	; >>
	.word	  0,          0,   50, equals		; ==
	.word	  0,          0,   50, notEqual		; !=
	.word	  0,          0,   60, lessOrEqual	; <=
	.word	  0,          0,   60, greaterOrEqual	; >=
	.word	  0,          0,   60, lessThan		; <
	.word	  0,          0,   60, greaterThan	; >
	.word	100,   positive,   80, add		; +
	.word	100,   negative,   80, subtract		; -
	.word	  0,          0,   90, multiply		; *
	.word	  0,          0,   90, divide		; /
	.word	  0,          0,   90, remainder	; %
	.word	  0,          0,   40, logicalAnd	; &
	.word	  0,          0,   20, logicalOr	; |
	.word	  0,          0,   30, logicalXor	; ^
	.word	100, complement,    0, 0		; ~
	.word	100, booleanNot,    0, 0		; !

; **************************
; *                        *
; *  General register use  *
; *                        *
; **************************

; r0 - r3 are for general use
; r4 - r12 are named and reserved for specific use
; sp, lr, pc have their normal use
rBot	.req	r4	; bottom of stack, value saved to reset stack at top of program
rNest	.req	r5	; subexpression nesting level, incremented on "(", decremented
			; on ")", must be 0 at end
rState	.req	r6	; parser state (0 or 1)
rOpr	.req	r7	; -> operator stack
rOpn	.req	r8	; -> operand stack
rToken	.req	r9	; -> current token
rOptab	.req	r10	; -> operator table
rSttab	.req	r11	; -> state table
rIO	.req	r12	; -> Console I/O locations

; **********************
; *                    *
; *  Start of Program  *
; *                    *
; **********************

	.global _start

_start:
; Load fixed registers
	ldr	rOptab, =opTable
	ldr	rSttab, =stateTable
	mov	rIO, #console
	mov	rBot, sp

; Top-level loop. The parser branches here when an expression has been completely parsed
; and executed, or when a syntax error is found.
top:	
	mov	rOpn, rBot			; Reset the stacks. Operand stack goes first.
	sub	rOpr, rOpn, #4*operandStackSize	; Now the operator stack.
	sub	sp,   rOpr, #4*operatorStackSize; The processor stack goes last.
	mov	rNest, #1			; Nest level starts at 1.
	mov	rState, #0			; Start in the A state
	str	rState, [rOpr, #-4]!		; Push a 0-marker on the operator stack.
	add	rToken, rIO, #tokens		; Get starting token address

; This is the top-level control for the parser state machine. Just 4 instructions!
next:	
	ldr	r0, [rToken], #4		    ; r0 = next token code
	ldr	r1, [rToken], #4		    ; r1 = next token value
	add	r0, rState, r0, lsl #1		; Get word index into state table
	ldr	pc, [rSttab, r0, lsl #2]	; Branch to state action

; **********************************
; *                                *
; *  Parser State Machine Actions  *
; *                                *
; **********************************
;
; Each of these actions should branch to next or top when done.

; Get the register value from the I/O console and push it onto the operand stack.
; Next state = B.
pushRegister:
	add r0, rIO, r1, LSL #2
	ldr r1, [r0]
	str r1, [rOpn, #-4]!
	mov rState, #1
	b next
	
; Push the literal value onto the operand stack. Next state = B.
pushLiteral:
	str r1, [rOpn, #-4]!
	mov rState, #1
	b	next

; If unary operator is defined, push action routine address and operator precedence onto
; the operand stack. Stay in state A. Otherwise it's a syntax error.
unaryOp:
	add r1, rOptab, r1, LSL #4
	ldr r0, [r1, #4] 
	cmp r0, #0
	mov rState, #0	
	beq syntaxError 
	
    ldr r0, [r1, #4]     ; get the action routine address
	str r0, [rOpr, #-4]!  ; push the action routin address onto the operator stack	
	ldr r0, [r1]      ; get the precendence value	
	str r0, [rOpr, #-4]!  ; push the precedence value onto the operator stack	
	
	b	next

; If binary operator is defined, execute operators of higher precedence, and then push action routine
; address and operator precedence onto the operand stack. Next state = A. Otherwise it's a syntax error.
binaryOp:
	add r1, rOptab, r1, LSL #4
	ldr r0, [r1, #8] 
	cmp r0, #0
	mov rState, #0
	beq syntaxError	
	
	bl execute
    ldr r0, [r1, #12]     ; get the action routine address
	str r0, [rOpr, #-4]!  ; push the action routin address onto the operator stack	
	ldr r0, [r1, #8]      ; get the precendence value	
	str r0, [rOpr, #-4]!  ; push the precedence value onto the operator stack

	b	next

; Push 0-marker onto operator stack and increment nesting level
subExpr:
	mov r0, #0
	str	r0, [rOpr, #-4]!		; Push a 0-marker on the operator stack.	
	add rNest, rNest, #1
	b	next

; Decrement nesting level. If nesting level <= 0 it's a syntax error.
; Execute all operators up to 0-marker, pop 0-marker.
endSub:
	sub rNest, rNest, #1
	cmp rNest, #0
	ble syntaxError
	
	mov r0, #0                  ;
	bl execute                  ; execute all operators up to 0-marker
	ldr r0, [rOpr], #4          ; pop the 0-marker

	b	next

; Decrement nesting level. If nesting level != 0 it's a syntax error.
; Execute all operators up to 0-marker, pop 0-marker.  Pop result from operand stack and display
; in E register of I/O console. Branch to top.
endExpr:
	sub rNest, rNest, #1
	cmp rNest, #0
	bne syntaxError
	 
	mov r0, #0                  ;
	bl execute                  ; execute all operators up to 0-marker
	ldr r0, [rOpr], #4          ; pop the 0-marker
	
	ldr r1, [rOpn], #4          ; pop the result from operand stack
	str r1, [rIO, #eReg]        ; display the result
	
	b	top
	
; Display 0 in E register of I/O console and branch to top.
syntaxError:	
	mov r1, #0
	str r1, [rIO, #eReg]		; Display 0 result and quit
	b	top
	
; ***********************
; *                     *
; *  Execute Operators  *
; *                     *
; ***********************

; The execute subroutie is called with a precedence value in r0. All operators on the
; operator stack are popped and executed until one is found of lower precedence than
; the value given in r0. All registers are preserved except for rOpr and rOpn, which
; are updated, and LR, which is undefined.
execute:
	stmdb sp!, {r0-r3,lr}
start:
	ldr r0, [sp]                 ;restore r0
	ldr r1, [rOpr]
	cmp r0, r1
                   
    ldmgeia sp!, {r0-r3,pc}        ; return here if r0>= r1
	
    ldr r1, [rOpr], #4           ; pop the value off the stack so we can use the action routine address
	mov lr, pc
    ldr pc, [rOpr], #4

	b start

; Here is how to call an action subroutine by popping the subroutine address
; off of the operator stack. You can replace the ldr with any *single* instruction
; that puts the address into the PC.
	mov	lr, pc			; Put the return address in LR
	ldr	pc, [rOpr], #4		; Pop the action routine address and call it
					; Subroutine will return here
; *********************************
; *                               *
; *  Operator Action Subroutines  *
; *                               *
; *********************************
;
; Binary operators pop their two operands from the operand stack and push the result.
; Unary operators pop their operand from the operand stack and push the result.

uShiftRight:
	ldr	r1, [rOpn], #4
	ldr	r0, [rOpn]
	mov	r0, r0, ASR r1
	str	r0, [rOpn]
	mov	pc, lr

shiftLeft:
	ldr	r1, [rOpn], #4
	ldr	r0, [rOpn]
	mov	r0, r0, LSL r1
	str	r0, [rOpn]
	mov	pc, lr

shiftRight:
	ldr	r1, [rOpn], #4
	ldr	r0, [rOpn]
	mov	r0, r0, LSR r1
	str	r0, [rOpn]
	mov	pc, lr

equals:
	ldr	r1, [rOpn], #4
	ldr	r0, [rOpn]
	mov r2, #0
	cmp	r0, r1
	moveq r2, #1
	str	r2, [rOpn]
	mov	pc, lr

notEqual:
	ldr	r1, [rOpn], #4
	ldr	r0, [rOpn]
	mov r2, #1
	cmp	r0, r1
	moveq r2, #0
	str	r2, [rOpn]
	mov	pc, lr

lessOrEqual:
	ldr	r1, [rOpn], #4
	ldr	r0, [rOpn]
	mov r2, #0
	cmp	r0, r1
	movle r2, #1
	str	r2, [rOpn]
	mov	pc, lr

greaterOrEqual:
	ldr	r1, [rOpn], #4
	ldr	r0, [rOpn]
	mov r2, #0
	cmp	r0, r1
	movge r2, #1
	str	r2, [rOpn]
	mov	pc, lr

lessThan:
	ldr	r1, [rOpn], #4
	ldr	r0, [rOpn]
	mov r2, #0
	cmp	r0, r1
	movlt r2, #1
	str	r2, [rOpn]
	mov	pc, lr

greaterThan:
	ldr	r1, [rOpn], #4
	ldr	r0, [rOpn]
	mov r2, #0
	cmp	r0, r1
	movgt r2, #1
	str	r2, [rOpn]
	mov	pc, lr

negative:
	ldr r1, [rOpn]
	rsb r1, r1, #0
	str r1, [rOpn]
	mov pc, lr
	
positive:
	mov	pc, lr

add:	
	ldr	r1, [rOpn], #4
	ldr	r0, [rOpn]
	add	r0, r0, r1
	str	r0, [rOpn]
	mov	pc, lr

subtract:
	ldr	r1, [rOpn], #4
	ldr	r0, [rOpn]
	sub	r0, r0, r1
	str	r0, [rOpn]
	mov	pc, lr


multiply: 
	ldr	r1, [rOpn], #4
	ldr	r0, [rOpn]
	mul	r2, r0, r1
	str	r2, [rOpn]
	mov	pc, lr

; divide r0 by r1
; result stored in r2
; remainder in r1
; r3 counts the steps of the divide loop

divide:
	ldr	r1, [rOpn], #4
	ldr	r0, [rOpn]
	
	;Take the product of the two operands and push it on the processor stack
	mul r3, r0, r1
	str r3, [sp, #-4]!
	
	;Take the absolute values of both operands	
	cmp r1, #0
	rsblt r1, r1, #0		;if r1 < 0
	cmp r0, #0
	rsblt r0, r0, #0		;if r0 < 0	
	
	clz r3, r1
	mov r1, r1, LSL r3

divideLoop:	
	mov r1, r1, LSR #1
	cmp r0, r1				;else if r3>=0
	adc r2, r2, r2
	subcs r0, r0, r1		;if r1 > r0
	subs r3, r3, #1	
	bne	divideLoop			;if r1 <= r0
	
	; If the program gets here, r2 holds the result of |r0|/|r1|
	; and r1 holds the remainder of |r0|/|r1|
	
	ldr r3, [sp], #4
	cmp r3, #0				
	rsblt r2, r2, #0		; if r0.r1 < 0, we have to flip the sign of the quotient
							; else we have the right quotient
	str	r2, [rOpn]
	mov	pc, lr	
	
	
	
; divide loop ends here
	
	
remainder:
	ldr	r1, [rOpn], #4
	ldr	r0, [rOpn]
	
	;push r0 onto the stack
	str r0, [sp, #-4]!
	
	;Take the absolute values of both operands	
	cmp r1, #0
	rsblt r1, r1, #0		;if r1 < 0
	cmp r0, #0
	rsblt r0, r0, #0		;if r0 < 0	
	
	clz r3, r1
	mov r1, r1, LSL r3

remainderLoop:	
	mov r1, r1, LSR #1
	cmp r0, r1				;else if r3>=0
	adc r2, r2, r2
	subcs r0, r0, r1		;if r1 > r0
	subs r3, r3, #1	
	bne	remainderLoop			;if r1 <= r0
	
	
	; If the program gets here, r2 holds the result of |r0|/|r1|
	; and r0 holds the remainder of |r0|/|r1|
	
	ldr r3, [sp], #4		; pop the original r0 and compare it to 0
	cmp r3, #0				
	rsblt r0, r0, #0		; if r0.r1 < 0, we have to flip the sign of the quotient
							; else we have the right quotient
	str	r0, [rOpn]
	mov	pc, lr	
	
	
	
; remainder loop ends here

logicalAnd:
	ldr	r1, [rOpn], #4
	ldr	r0, [rOpn]
	and	r2, r0, r1
	str	r2, [rOpn]
	mov	pc, lr

logicalOr:
	ldr	r1, [rOpn], #4
	ldr	r0, [rOpn]
	ORR	r2, r0, r1
	str	r2, [rOpn]
	mov	pc, lr

logicalXor:
	ldr	r1, [rOpn], #4
	ldr	r0, [rOpn]
	EOR	r2, r0, r1
	str	r2, [rOpn]
	mov	pc, lr

complement:
	ldr r1, [rOpn]
	mvn r1, r1
	str r1, [rOpn]
	mov pc, lr	

booleanNot:
	ldr r0, [rOpn]
	mov r1, #0
	cmp r0, #0
	moveq r1, #1
	str r1, [rOpn]
	mov pc, lr	
