QHOW:
        PUSH DE                         ;*** ERROR "HOW?" ***
AHOW:
        LD DE,HOW
        JR ERROR_ROUTINE
	
QSORRY:
        PUSH DE                         ;*** QSORRY ***
ASORRY:
        LD DE,SORRY                     ;*** ASORRY ***
        JR ERROR_ROUTINE

QWHAT:
        PUSH DE                         ;*** QWHAT ***
AWHAT:
        LD DE,WHAT                      ;*** AWHAT ***
	JR ERROR_ROUTINE
	
ERROR_ROUTINE:
        SUB A                           ;*** ERROR ***
        CALL PRTSTG                     ;PRINT 'WHAT?', 'HOW?'
        POP DE                          ;OR 'SORRY'
        LD A,(DE)                       ;SAVE THE CHARACTER
        PUSH AF                         ;AT WHERE OLD DE ->
        SUB A                           ;AND PUT A 0 THERE
        LD (DE),A
        LD HL,(CURRNT)                  ;GET CURRENT LINE #
        PUSH HL
        LD A,(HL)                       ;CHECK THE VALUE
        INC HL
        OR (HL)
        POP DE
        JP Z,RSTART                     ;IF ZERO, JUST RESTART
        LD A,(HL)                       ;IF NEGATIVE,
        OR A
        JP M,INPERR                     ;REDO INPUT
        CALL PRTLN                      ;ELSE PRINT THE LINE
        DEC DE                          ;UPTO WHERE THE 0 IS
        POP AF                          ;RESTORE THE CHARACTER
        LD (DE),A
        LD A,3FH                        ;PRINT A "?"
        RST 10H
        SUB A                           ;AND THE REST OF THE
        CALL PRTSTG                     ;LINE
        JP RSTART                       ;THEN RESTART

HOW:    DB "HOW?",CR
WHAT:   DB "WHAT?",CR
SORRY:  DB "SORRY",CR
