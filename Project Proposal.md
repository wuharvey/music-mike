Playlist
=====

### Intro
Western music is usually notated on a five-line staff, on which **notes** are given a **duration** based on symbol type, and **pitch** based on location in the staff. Composers can use proprietary software such as Sibelius or Finale to manipulate a virtual five-line staff through mouse clicks or keyboard gestures. Fans of computer music might instead use music synthesis libraries to programmatically create music in languages such as C++, but such libraries can be unintuitive for musicians unfamiliar with signals and waves.

We propose MUSICMIKE, a strongly typed, high-level, functional programming language, to give users an alternative option in music creation. MUSICMIKE is designed for users to create music based on varied manipulations of short patterns. We owe this idea to Note Hashtag, a previous project completed in COMS W4115. However, unlike Note Hashtag, MUSICMIKE is **modal** rather than **key-based**, such that mode is user-defined or pre-programmed. Furthermore, lists - treated as the fundamental building block of music - are manipulated with special list operators (syntactic sugar) which create an intuitive interface for musicians based on traditional staff notation.

### Design Ethos
The most basic unit in music is a **note**, which can be decomposed into pitch and duration. A simple melody can thus be described as two lists: one list of pitches and another of durations. A **chord** is a collection of notes played simultaneously. 

A set of pitches is defined as a **mode**. All modes are subsets of the chromatic scale, which contains all twelve pitch classes used in Western music. Most music constrains the pitches of its notes to a small set of familiar modes, such as the major and minor scales. The sound of a **chord** is very much dependent on the **mode** that its notes come from.

MUSICMIKE is based on the following observations regarding Western music: one, that Western music is fundamentally  **chordal** and **modal**. Two, that Western music is repetitive and manipulative: simple building blocks of music are modified, then repeated multiple times in a piece. Finally, and most importantly, that these simple building blocks can described using lists and altered using a functional paradigm. 

Our model of music software is based off tunes - a collection of lists that specify the relative pitches, rhythms, and mode, of the tune. Tunes can be manipulated both in temporal placement in ultimately generated .wav file and in actual content through use of map() (applying functions to the tune) and plot() (converting the tune into data such that it is convertible to a .wav file). 

This system yields several advantages. 1) One can easily transpose between different keys by applying functions on the tune’s list of pitches; though the absolute notes of the scale will change, the mapping of mode will ensure the tune remains the same. 2) One can change the mode in tune while keeping other parameters constant, thus altering a melody in interesting ways (for example, altering “Happy birthday” so it’s rendered in a minor key).

By taking advantage of music theory and the relationships of notes within scales and modes with each other along with the relationships between notes from different scales and modes, MUSICMIKE can create a more intuitive programming language for musicians. 


	// Happy Birthday
	mode = Cmajor; 
	// Alternatively could define as major=[C D E F G A B];

	rhythm = [[ 8o 16 4 4 4 2 8o 16 4 4 4 2 1 ]]; 
	// o is an operator that “dots” a rhythm unit. 

	pitches = <5 5 6 5 1 7 5 5 6 5 2 1 0>; 
	// Zero notates a rest.

	melody = wrap(mode, pitches, rhythm);
	// Function arguments in parentheses and separated by commas, “C4” is string representing start note.

	fun Ascend x = x#;
	new_pitches = map(Ascend, pitches);
	new_melody = wrap(mode, new_pitches, rhythm);
	//now in key of c# major

	fun Augment x = x*2;
	new_rhythm = map(Augment, rhythm);
	more_melody = wrap(mode, pitches, new_rhythm);
	//now twice as long as the original melody


### Team Members

| Name               | UNI     | Role                |
|--------------------|---------|---------------------|
| Husam Abdul-Kafi   | hsa2136 | System Architect   |
| Mounika Bodapati   | lmb2254 | Manager             |
| Kaitlin Pet        | khp210  | Tester              |
| Harvey Wu          | hw2473  | Language Guru       |
