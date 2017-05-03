#include "stdafx.h"
#include "stdlib.h"
#include <iostream>


#include "CFugueLib.h"


    int main()
    {
        CFugue::SaveAsMidiFile(_T("Cq Dw Ex"), "MidiOutput.midi"); // Save the Music Notes to Midi file directly, without playing
        return 0;
}
