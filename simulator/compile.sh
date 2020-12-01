#!/bin/bash

arm-none-eabi-gcc \
    -fpic -pedantic -pedantic-errors -nostdlib \
    -ffreestanding -fomit-frame-pointer -mcpu=arm1176jzf-s \
    -Wall \
    -c -Os try.c

arm-none-eabi-ld \
    try.o -Tldscript -o try
