INIT:
        DI

        SUB A
        LD DE,MSG1          ;PRINT THE BOOT MESSAGES
        CALL PRTSTG
        LD DE,MSG2
        CALL PRTSTG
        LD HL,START
        LD (RANPNT),HL
        LD HL,TXTBGN
        LD (TXTUNF),HL
        JP RSTART

MSG1:   DB   ESC,"[2J",ESC,"[H"         ;SCREEN CLEAR
        DB   'Z80 TINY BASIC 2.0g',CR       ;BOOT MESSAGE
MSG2:   DB   'PORTED BY D. GABBARD, 2017',CR

