#include "stdafx.h"
#include "stdlib.h"
#include <iostream>

#include "CFugueLib.h"

#ifdef _UNICODE
typedef std::wstring tstring;
#else
typedef std::string tstring;
#endif


int main(int argc, char* argv[]) {
	std::string a(argv[1]);
	std::cout << a << "\n";
	std::wstring widestr = std::wstring(a.begin(), a.end());
	const wchar_t* music = widestr.c_str();
    CFugue::SaveAsMidiFile(music, "MidiOutput.midi"); // Save the Music Notes to Midi file directly, without playing
        return 0;
}
