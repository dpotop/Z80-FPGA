all: prog-bin.h

prog-bin.h: basic.bin convert.exe
	./convert.exe > prog-bin.h

convert.exe: convert.c
	gcc convert.c -o convert.exe

basic.bin: basic.asm ascii.asm io.asm
	z80asm basic.asm -o basic.bin -l 2> basic.lst

clean:
	rm -f basic.bin basic.lst convert.exe prog-bin.h
