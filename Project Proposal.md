Playlist
=====

### Intro
Western music is usually notated on a five-line staff, on which notes are given a duration based on symbol type, and pitch based on position in said staff. Modern composers, instead of manually writing on staff paper, can use proprietary software such as Sibelius or Finale to aid in their music-making - such programs facilitate the manipulation of a virtual score through mouse clicks or keyboard gestures. (need more here)

Thus we propose PLAYLIST, a high-level, imperative programming language, to give users an alternative option in music creation.  

### Design Ethos
Music is based on notes, which can be decomposed into two components: pitch and duration. These can be expressed mathematically using numbers. A melody can thus be described as two lists: one list of pitches and another of durations.Playlist's advantage lies in intuitive list declaration and manipulation, taking cues from OCaml and Python respectively.

(A chordal accompaniment for this melody can also be described using two lists. One is a list of chords - which are lists of notes -  and another list of durations for these chords.)

### Example Program

There is no such thing as an "absolute" pitch value in Playlist. All notes are defined relative to a scale (for example, C major). However, accidentals are still easy to notate. Simply prefix an integer in a pitch list with a plus or minus sign and the note will move up or down a half step according to the chromatic scale. Suffix an integer in a pitch list with a plus or minus sign and it will move up or down an octave. Similarly, adding a period to an integer in a rhythm list will "dot" that note value (multiply it by 1.5). Double dots and accidentals are also supported.

	// Happy Birthday
	using scale C; 
	rhythm = [8. 16 4 4 4 2 8. 16 4 4 4 2 1];
	pitches = [5 5 6 5 1+ 7 5 5 6 5 2+ 1+ 0]; // Zero notates a rest.
	melody = map(pitches, rhythm) // Function arguments in parentheses and separated by commas.

### Team Members

| Name               | UNI     | Role                |
|--------------------|---------|---------------------|
| Husam Abdul-Kafi   | hsa2136 | System Architect 	 |
| Mounika Bodapati   | lmb2254 | Manager             |
| Kaitlin Pet        | khp210  | Tester              |
| Harvey Wu          | hw2473  | Language Guru       |