;*************************************************************
;
; *** TABLES *** DIRECT *** & EXEC ***
;
; THIS SECTION OF THE CODE TESTS A STRING AGAINST A TABLE.
; WHEN A MATCH IS FOUND, CONTROL IS TRANSFERED TO THE SECTION
; OF CODE ACCORDING TO THE TABLE.
;
; AT 'EXEC', DE SHOULD POINT TO THE STRING AND HL SHOULD POINT
; TO THE TABLE-1.  AT 'DIRECT', DE SHOULD POINT TO THE STRING.
; HL WILL BE SET UP TO POINT TO TAB1-1, WHICH IS THE TABLE OF
; ALL DIRECT AND STATEMENT COMMANDS.
;
; A '.' IN THE STRING WILL TERMINATE THE TEST AND THE PARTIAL
; MATCH WILL BE CONSIDERED AS A MATCH.  E.G., 'P.', 'PR.',
; 'PRI.', 'PRIN.', OR 'PRINT' WILL ALL MATCH 'PRINT'.
;
; THE TABLE CONSISTS OF ANY NUMBER OF ITEMS.  EACH ITEM
; IS A STRING OF CHARACTERS WITH BIT 7 SET TO 0 AND
; A JUMP ADDRESS STORED HI-LOW WITH BIT 7 OF THE HIGH
; BYTE SET TO 1.
;
; END OF TABLE IS AN ITEM WITH A JUMP ADDRESS ONLY.  IF THE
; STRING DOES NOT MATCH ANY OF THE OTHER ITEMS, IT WILL
; MATCH THIS NULL ITEM AS DEFAULT.
;*************************************************************

	;; This is a weird macro - it's like a DW, but:
	;; - the bytes are reversed (Z80 is little endian, whereas
	;;   here the first byte is the most significant)
	;; - the most significant bit is artificially set, probably
	;;   to allow some identification through algorithms.
	;; Ideally, I should use a regular DW such as
	;; "DW   WHERE+(1<<15)" but for now I'll let it like it is.
DWA:    MACRO WHERE
	DB   (WHERE >> 8) + 128	
	DB   WHERE & 0FFH
        ENDM
	
TAB1:                                   ;DIRECT COMMANDS
        DB 'LIST'
        DWA LIST
        DB 'RUN'
        DWA RUN
        DB 'NEW'
        DWA NEW
TAB2:                                   ;DIRECT/STATEMENT
        DB 'NEXT'
        DWA NEXT
        DB 'LET'
        DWA LET
        DB 'IF'
        DWA IFF
        DB 'GOTO'
        DWA GOTO
        DB 'GOSUB'
        DWA GOSUB
        DB 'RETURN'
        DWA RETURN
        DB 'REM'
        DWA REM
        DB 'FOR'
        DWA FOR
        DB 'INPUT'
        DWA INPUT
        DB 'PRINT'
        DWA PRINT
        DB 'STOP'
        DWA STOP
        DWA DEFLT
TAB4:                                   ;FUNCTIONS
        DB 'RND'
        DWA RND
        DB 'ABS'
        DWA ABS
        DB 'SIZE'
        DWA SIZE
        DWA XP40
TAB5:                                   ;"TO" IN "FOR"
        DB 'TO'
        DWA FR1
        DWA QWHAT
TAB6:                                   ;"STEP" IN "FOR"
        DB 'STEP'
        DWA FR2
        DWA FR3
TAB8:                                   ;RELATION OPERATORS
        DB '>='
        DWA XP11
        DB '#'
        DWA XP12
        DB '>'
        DWA XP13
        DB '='
        DWA XP15
        DB '<='
        DWA XP14
        DB '<'
        DWA XP16
        DWA XP17

DIRECT: LD HL,TAB1-1                   ;*** DIRECT ***
EXEC:                                   ;*** EXEC ***
EX0:    RST 28H                         ;IGNORE LEADING BLANKS
        PUSH DE                         ;SAVE POINTER
EX1:
        LD A,(DE)                       ;IF FOUND '.' IN STRING
        INC DE                          ;BEFORE ANY MISMATCH
        CP 23H                          ;WE DECLARE A MATCH
        JR Z,EX3
        INC HL                          ;HL->TABLE
        CP (HL)                         ;IF MATCH, TEST NEXT
        JR Z,EX1
        LD A,7FH                        ;ELSE SEE IF BIT 7
        DEC DE                          ;OF TABLE IS SET, WHICH
        CP (HL)                         ;IS THE JUMP ADDR. (HI)
        JR C,EX5                        ;C:YES, MATCHED
EX2:
        INC HL                          ;NC:NO, FIND JUMP ADDR.
        CP (HL)
        JR NC,EX2
        INC HL                          ;BUMP TO NEXT TAB. ITEM
        POP DE                          ;RESTORE STRING POINTER
        JR EX0                          ;TEST AGAINST NEXT ITEM
EX3:
        LD A,7FH                        ;PARTIAL MATCH, FIND
EX4:
        INC HL                          ;JUMP ADDR., WHICH IS
        CP (HL)                         ;FLAGGED BY BIT 7
        JR NC,EX4
EX5:
        LD A,(HL)                       ;LOAD HL WITH THE JUMP
        INC HL                          ;ADDRESS FROM THE TABLE
        LD L,(HL)
        AND 7FH                         ;MASK OFF BIT 7
        LD H,A
        POP AF                          ;CLEAN UP THE GABAGE
        JP (HL)                         ;AND WE GO DO IT
