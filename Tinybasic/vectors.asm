	;; TODO: for now, NMI66 is off by 1 byte!!!!
	;; Interrupt vectors, which must be at fixed
	;; addresses:
	;; - 0x00 for the boot address
	;; - 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38 for the other RST
	;;   vectors. Of these 0x38 may be used with INT, in case we
	;;   don't use vectored interrupts.
	;; - 0x66 for the NMI vector.

START:
        LD SP,STACK                     ;*** COLD START ***
        LD A,0FFH
        JP INIT

RST08:  EX (SP),HL                      ;*** TSTC OR RST 08H ***
        RST 28H                         ;IGNORE BLANKS AND
        CP (HL)                         ;TEST CHARACTER
        JP TC1                          ;REST OF THIS IS AT TC1

CRLF:
        LD A,CR                         ;*** CRLF ***

RST10:
	JP OUTC                         ;REST OF THIS AT OUTC
	NOP				; Need these NOPs in order to
	NOP				; align RST18
	NOP
	NOP
	NOP

RST18:  CALL EXPR2                      ;*** EXPR OR RST 18H ***
        PUSH HL                         ;EVALUATE AN EXPRESSION
        JP EXPR1                        ;REST OF IT AT EXPR1
        NOP

RST20:  LD A,H                          ;*** COMP OR RST 20H ***
        CP D                            ;COMPARE HL WITH DE
        RET NZ                          ;RETURN CORRECT C AND
        LD A,L                          ;Z FLAGS
        CP E                            ;BUT OLD A IS LOST
        RET
        NOP
	NOP

SS1:
RST28:  LD A,(DE)                       ;*** IGNBLK/RST 28H ***
        CP 20H                          ;IGNORE BLANKS
        RET NZ                          ;IN TEXT (WHERE DE->)
        INC DE                          ;AND RETURN THE FIRST
        JP SS1                          ;NON-BLANK CHAR. IN A

RST30:  POP AF                          ;*** FINISH/RST 30H ***
        CALL FIN                        ;CHECK END OF COMMAND
        JP QWHAT                        ;PRINT "WHAT?" IF WRONG
        NOP

RST38:  RST 28H                         ;*** TSTV OR RST 38H ***
        SUB 40H                         ;TEST VARIABLES
        RET C                           ;C:NOT A VARIABLE
        JR NZ,TV1                       ;NOT "@" ARRAY
        INC DE                          ;IT IS THE "@" ARRAY
        CALL PARN                       ;@ SHOULD BE FOLLOWED
        ADD HL,HL                       ;BY (EXPR) AS ITS INDEX
        JP C,QHOW                       ;IS INDEX TOO BIG?
        PUSH DE                         ;WILL IT OVERWRITE
        EX DE,HL                        ;TEXT?
        CALL SIZE                       ;FIND SIZE OF FREE
        RST 20H                         ;AND CHECK THAT
        JP C,ASORRY                     ;IF SO, SAY "SORRY"
        LD HL,VARBGN                    ;IF NOT GET ADDRESS
        CALL SUBDE                      ;OF @(EXPR) AND PUT IT
        POP DE                          ;IN HL
        RET                             ;C FLAG IS CLEARED

TV1:
        CP 1BH                          ;NOT @, IS IT A TO Z?
        CCF                             ;IF NOT RETURN C FLAG
        RET C
        INC DE                          ;IF A THROUGH Z
        LD HL,VARBGN                    ;COMPUTE ADDRESS OF
        RLCA                            ;THAT VARIABLE
        ADD A,L                         ;AND RETURN IT IN HL
        LD L,A                          ;WITH C FLAG CLEARED
        LD A,00H
        ADC A,H
        LD H,A
        RET

NMI66:                                  ;MUST BE AT 66H
        RETI

TC1:
        INC HL                          ;COMPARE THE BYTE THAT
        JR Z,TC2                        ;FOLLOWS THE RST INST.
        PUSH BC                         ;WITH THE TEXT (DE->)
        LD C,(HL)                       ;IF NOT =, ADD THE 2ND
        LD B,00H                        ;BYTE THAT FOLLOWS THE
        ADD HL,BC                       ;RST TO THE OLD PC
        POP BC                          ;I.E., DO A RELATIVE
        DEC DE                          ;JUMP IF NOT =

TC2:
        INC DE                          ;IF =, SKIP THOSE BYTES
        INC HL                          ;AND CONTINUE
        EX (SP),HL
        RET

TSTNUM:
        LD HL,0000H                     ;*** TSTNUM ***
        LD B,H                          ;TEST IF THE TEXT IS
        RST 28H                         ;A NUMBER

TN1:
        CP 30H                          ;IF NOT, RETURN 0 IN
        RET C                           ;B AND HL
        CP 3AH                          ;IF NUMBERS, CONVERT
        RET NC                          ;TO BINARY IN HL AND
        LD A,0F0H                       ;SET B TO # OF DIGITS
        AND H                           ;IF H>255, THERE IS NO
        JP NZ,QHOW                      ;ROOM FOR NEXT DIGIT
        INC B                           ;B COUNTS # OF DIGITS
        PUSH BC
        LD B,H                          ;HL=10*HL+(NEW DIGIT)
        LD C,L
        ADD HL,HL                       ;WHERE 10* IS DONE BY
        ADD HL,HL                       ;SHIFT AND ADD
        ADD HL,BC
        ADD HL,HL
        LD A,(DE)                       ;AND (DIGIT) IS FROM
        INC DE                          ;STRIPPING THE ASCII
        AND 0FH                         ;CODE
        ADD A,L
        LD L,A
        LD A,00H
        ADC A,H
        LD H,A
        POP BC
        LD A,(DE)                       ;DO THIS DIGIT AFTER
        JP P,TN1                        ;DIGIT. S SAYS OVERFLOW

