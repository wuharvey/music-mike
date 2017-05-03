#include "stdafx.h"
#include "stdlib.h"
#include <iostream>
#include <string>
using namespace std;

#include "CFugueLib.h"


    int main(int argc, char* argv[])
    {
	string music(argv[1]);
	cout << music << "\n";
        CFugue::SaveAsMidiFile(_T(music), "MidiOutput.midi"); // Save the Music Notes to Midi file directly, without playing
        return 0;
}
