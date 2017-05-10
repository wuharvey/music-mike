#include <unistd.h>

int make_midi(char * buffer, char * name){
  execl("./testCFugueLib", "./testCFugueLib", name, buffer, (char *)0);
  return 0;
}
