#include <stdio.h>
#include <stdlib.h>

#include <editline/readline.h>

int main(int argc, char** argv) {
  puts("Lispy version 0.0.1");
  puts("Press Ctrl+C to exit");

  while (1) {
    char* in = readline("lispy> ");
    add_history(in);

    printf("No, you're a %s\n", in);
    free(in);
  }
  return 0;
}
