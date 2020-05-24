![z80 and its Arduino umbilical cord](https://github.com/dpotop/Z80-hobby/blob/master/DSC_0046.JPG&s=200)

# Z80-based hobby computer

This github project will accompany my effort to build 
a working z80-based home computer. The objective is largely 
educational in scope, as I hope it will help me explain my son
how computers are built and work.

Building such a home computer used to be, until quite recently, 
a difficult endeavor, as it required:
1. building the minimal hardware (CPU+ROM+RAM).
2. building the I/O devices (to the least, some form of UART-based I/O).
3. writing the minimal software allowing some form of I/O.
4. Transferring basic software into ROM.
5. writing meaningful SW.

But the main difficulty was that phases 1-4 had to be performed
as a single step, with few possibilities for per-phase feedback
and debugging. Thus, in the absence of advanced (and expensive)
equipment, building such a home computer required a pre-assembled
kit, and the only thing to do some soldering.

However, this is no longer true. In developing this project, I take 
advantage of the ability to use microcontroller boards (Arduino in 
my case) to partially or fully control the execution of hardware.

Links for the UART step:
* https://z80project.wordpress.com/2014/08/21/uart-serial-terminal-output-test/
* https://electronics.stackexchange.com/questions/94658/using-uart-clock-3-6864-mhz-for-a-microcontroller
