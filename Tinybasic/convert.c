#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

void print_hex(unsigned char c) {
  printf("0x") ;
  printf("%x",c/16) ;
  printf("%x",c%16) ;
}

int main(void) {
  FILE* fis = fopen("basic.bin","r") ;
  if(fis == NULL) {
    printf("Cannot open file basic.bin\n") ;
    exit(0) ;
  }
  fseek(fis, 0L, SEEK_END);
  int fsize = ftell(fis);
  fseek(fis, 0L, SEEK_SET);
  printf("// File size: %d octets\n",fsize) ;
  printf("unsigned char ram[4096] = {\n") ;
  int char_cnt = 0 ;
  printf("\t/*%4x*/ ",char_cnt) ;
  for(int char_cnt = 0 ; char_cnt < fsize ; char_cnt++) {
    unsigned char c ;
    int tmp ;
    do {
      tmp = fread(&c,1,1,fis) ;
    } while(tmp == 0) ;
    if(tmp != 1) printf("\nERROR!!!!!\n") ;
    print_hex(c) ;
    if(char_cnt + 1 != fsize) printf(", ") ;
    if(char_cnt % 16 == 15)   printf("\n\t/*%4x*/ ",char_cnt+1) ;
  }
  printf("\n};\n") ;
  return 0 ;
}
