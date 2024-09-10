/* main.c: C wrapper for our assembly program */
#include <stdio.h>
#include <stdlib.h> 

void print_int (int x) {
    printf ("%d\n", x);
}

int main(int argc, char *argv[]) {
  int n = atoi(argv[1]);
  int sum = 0;
  int c;
  for (int i = 0; i < n; i ++){
    printf("Enter integer #%d:\n", i);
    scanf("%d", &c);
    sum += c;
  }
  
  print_int(sum);
  return 0; 
}
