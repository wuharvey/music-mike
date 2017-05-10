#include <unistd.h>

int make_midi(char * buffer, char * name){
  execl("./testCFugueLib", "./testCFugueLib", buffer, name, (char *)0);
  return 0;
}
