	;; All of this must be in the RAM. All code and data prior
	;; must be in the ROM.
LSTROM:
VARBGN: 	DS  55        ;VARIABLE @(0)
BUFFER: 	DS  64        ;INPUT BUFFER
BUFEND: 	DS   1        ;BUFFER ENDS
CURRNT:         DS   2	      ;POINTS FOR OUTPUT
STKGOS:         DS   2        ;SAVES SP IN 'GOSUB'
VARNXT:         DS   2	      ;TEMP STORAGE
STKINP:         DS   2        ;SAVES SP IN 'INPUT'
LOPVAR:         DS   2        ;'FOR' LOOP SAVE AREA
LOPINC:         DS   2        ;INCREMENT
LOPLMT:         DS   2        ;LIMIT
LOPLN:          DS   2        ;LINE NUMBER
LOPPT:          DS   2        ;TEXT POINTER
RANPNT:         DS   2        ;RANDOM NUMBER POINTER
TXTUNF:         DS   2        ;->UNFILLED TEXT AREA
TXTBGN:         DS 1024        ;TEXT SAVE AREA BEGINS
TXTEND:                       ;TEXT SAVE AREA ENDS
STKLMT:	        DS 1024	      ;STACK BASE
STACK:	        DS   1	      ;STACK POINTER
