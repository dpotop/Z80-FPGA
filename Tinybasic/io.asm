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

