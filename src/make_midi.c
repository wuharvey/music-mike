#include <unistd.h>

int make_midi(char * buffer){
  execl("./testCFugueLib", "./testCFugueLib", buffer, (char *)0);
  return 0;
}
