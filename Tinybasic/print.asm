;*************************************************************
;
; *** PRTSTG *** QTSTG *** PRTNUM *** & PRTLN ***
;
; 'PRTSTG' PRINTS A STRING POINTED BY DE.  IT STOPS PRINTING
; AND RETURNS TO CALLER WHEN EITHER A CR IS PRINTED OR WHEN
; THE NEXT BYTE IS THE SAME AS WHAT WAS IN A (GIVEN BY THE
; CALLER).  OLD A IS STORED IN B, OLD B IS LOST.
;
; 'QTSTG' LOOKS FOR A BACK-ARROW, SINGLE QUOTE, OR DOUBLE
; QUOTE.  IF NONE OF THESE, RETURN TO CALLER.  IF BACK-ARROW,
; OUTPUT A CR WITHOUT A LF.  IF SINGLE OR DOUBLE QUOTE, PRINT
; THE STRING IN THE QUOTE AND DEMANDS A MATCHING UNQUOTE.
; AFTER THE PRINTING THE NEXT 3 BYTES OF THE CALLER IS SKIPPED
; OVER (USUALLY A JUMP INSTRUCTION.
;
; 'PRTNUM' PRINTS THE NUMBER IN HL.  LEADING BLANKS ARE ADDED
; IF NEEDED TO PAD THE NUMBER OF SPACES TO THE NUMBER IN C.
; HOWEVER, IF THE NUMBER OF DIGITS IS LARGER THAN THE # IN
; C, ALL DIGITS ARE PRINTED ANYWAY.  NEGATIVE SIGN IS ALSO
; PRINTED AND COUNTED IN, POSITIVE SIGN IS NOT.
;
; 'PRTLN' PRINTS A SAVED TEXT LINE WITH LINE # AND ALL.
;*************************************************************

PRTSTG:
        LD B,A                          ;*** PRTSTG ***
PS1:
        LD A,(DE)                       ;GET A CHARACTER
        INC DE                          ;BUMP POINTER
        CP B                            ;SAME AS OLD A?
        RET Z                           ;YES, RETURN
        RST 10H                         ;NO, NEXT
        CP CR                           ;WAS IT A CR?
        JR NZ,PS1                       ;NO, NEXT
        RET                             ;YES, RETURN
QTSTG:
        RST 08H                         ;*** QTSTG ***
        DB '"'
        DB QT3-$-1
        LD A,22H                        ;IT IS A "
QT1:
        CALL PRTSTG                     ;PRINT UNTIL ANOTHER
        CP CR                           ;WAS LAST ONE A CR?
        POP HL                          ;RETURN ADDRESS
        JP Z,RUNNXL                     ;WAS CR, RUN NEXT LINE
QT2:
        INC HL                          ;SKIP 3 BYTES ON RETURN
        INC HL
        INC HL
        JP (HL)                         ;RETURN
QT3:
        RST 08H                         ;IS IT A '?
        DB 27H
        DB QT4-$-1
        LD A,27H                        ;YES, DO THE SAME
        JR QT1                          ;AS IN ""
QT4:
        RST 08H                         ;IS IT BACK-ARROW?
        DB 5FH
        DB QT5-$-1
        LD A,8DH                        ;YES, CR WITHOUT LF
        RST 10H                         ;DO IT TWICE TO GIVE
        RST 10H                         ;TTY ENOUGH TIME
        POP HL                          ;RETURN ADDRESS
        JR QT2
QT5:
        RET                             ;NONE OF ABOVE
;
PRTNUM:
        LD B,00H                        ;*** PRTNUM ***
        CALL CHKSGN                     ;CHECK SIGN
        JP P,PN1                        ;NO SIGN
        LD B,'-'                        ;B=SIGN
        DEC C                           ;'-' TAKES SPACE
PN1:
        PUSH DE                         ;SAVE
        LD DE,000AH                     ;DECIMAL
        PUSH DE                         ;SAVE AS FLAG
        DEC C                           ;C=SPACES
        PUSH BC                         ;SAVE SIGN & SPACE
PN2:
        CALL DIVIDE                     ;DIVIDE HL BY 10
        LD A,B                          ;RESULT 0?
        OR C
        JR Z,PN3                        ;YES, WE GOT ALL
        EX (SP),HL                      ;NO, SAVE REMAINDER
        DEC L                           ;AND COUNT SPACE
        PUSH HL                         ;HL IS OLD BC
        LD H,B                          ;MOVE RESULT TO BC
        LD L,C
        JR PN2                          ;AND DIVIDE BY 10
PN3:
        POP BC                          ;WE GOT ALL DIGITS IN
PN4:
        DEC C                           ;THE STACK
        LD A,C                          ;LOOK AT SPACE COUNT
        OR A
        JP M,PN5                        ;NO LEADING BLANKS
        LD A,20H                        ;LEADING BLANKS
        RST 10H
        JR PN4                          ;MORE?
PN5:
        LD A,B                          ;PRINT SIGN
        OR A
        CALL NZ,0010H
        LD E,L                          ;LAST REMAINDER IN E
PN6:
        LD A,E                          ;CHECK DIGIT IN E
        CP 0AH                          ;10 IS FLAG FOR NO MORE
        POP DE
        RET Z                           ;IF SO, RETURN
        ADD A,30H                       ;ELSE, CONVERT TO ASCII
        RST 10H                         ;PRINT THE DIGIT
        JR PN6                          ;GO BACK FOR MORE
PRTLN:
        LD A,(DE)                       ;*** PRTLN ***
        LD L,A                          ;LOW ORDER LINE #
        INC DE
        LD A,(DE)                       ;HIGH ORDER
        LD H,A
        INC DE
        LD C,04H                        ;PRINT 4 DIGIT LINE #
        CALL PRTNUM
        LD A,20H                        ;FOLLOWED BY A BLANK
        RST 10H
        SUB A                           ;AND THEN THE NEXT
        CALL PRTSTG
        RET
