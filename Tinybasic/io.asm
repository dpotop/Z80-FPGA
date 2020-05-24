;*************************************************************
;
; *** OUTC *** & CHKIO ***
;
; THESE ARE THE ONLY I/O ROUTINES IN TBI.
; 'OUTC' WILL OUTPUT THE BYTE IN A.  IF THAT IS A CR, A LF IS
; ALSO SENT OUT.  ONLY THE FLAGS MAY BE CHANGED AT RETURN.
; ALL REG. ARE RESTORED.
;
; 'CHKIO' CHECKS THE INPUT.  IF NO INPUT, IT WILL RETURN TO
; THE CALLER WITH THE Z FLAG SET.  IF THERE IS INPUT, Z FLAG
; IS CLEARED AND THE INPUT BYTE IS IN A.  IF A CONTROL-C IS READ,
;'CHKIO' WILL RESTART TBI AND DO NOT RETURN TO THE CALLER.
;*************************************************************

SerialPort:     EQU     010H            ; This the serial I/O port

OUTC:
        OUT (SerialPort),A      ;SEND THE BYTE
        CP CR
	JP NZ,OUTC2
        LD A,LF
	OUT (SerialPort),A
        LD A,CR
OUTC2:	
        RET

CHKIO:
	; DPB: read one character. If it's zero,
	; it means no input is available
	IN A,(SerialPort)
	AND A   			; Check if it's zero
        JP Z,CHKIO2 			; Return if no character
        CP 03H                          ; IS IT CONTROL-C?
        JP Z,RSTART                     ; YES, RESTART TBI
CHKIO2:	RET

