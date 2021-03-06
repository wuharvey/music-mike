ntroduction Section
### Introduction
Western music is usually notated on a five-line staff, on which **notes** are given a **duration** based on symbol type, and **pitch** based on location in the staff. Composers can use proprietary software such as Sibelius or Finale to manipulate a virtual five-line staff through mouse clicks or keyboard gestures. Fans of computer music might instead use music synthesis libraries to programmatically create music in languages such as C++, but such libraries can be unintuitive for musicians unfamiliar with signals and waves.

We propose Music-mike, a compiled, strongly typed, programming language, to give users an alternative option in music creation. Music-mike is designed for users to create music based on varied manipulations of short patterns. We owe this idea to Note Hashtag, a previous project completed in COMS W4115. However, unlike Note Hashtag, Music-mike is **modal** rather than **key-based**. Furthermore, lists - treated as the fundamental building block of music - are manipulated with special list operators (syntactic sugar) which create an intuitive interface based on traditional staff notation.

### Design Ethos
The most basic unit in music is a **note**, which can be decomposed into pitch and duration. A simple melody can thus be described as two lists: one list of pitches and another of durations. A **chord** is a collection of notes played simultaneously.

A set of pitches is defined as a **mode**. All modes are subsets of the chromatic scale, which contains all twelve pitch classes used in Western music. Most music constrains the pitches of its notes to a small set of familiar modes, such as the major and minor scales. The sound of a **chord** is very much dependent on the **mode** that its notes come from.

Music-mike is based on the following observations regarding Western music: one, that Western music is fundamentally  **chordal** and **modal**. Two, that Western music is repetitive and manipulative: simple building blocks of music are modified, then repeated multiple times in a piece. Finally, and most importantly, that these simple building blocks can described using lists and altered using a functional paradigm.

Language Tutorial Section
LRM Section
Project Plan Section
A. Process
  i. Planning
  Our team met regularly twice a week on Wednesdays to meet with our TA Jacob Graff and
  on Sundays to work together as a team, debrief and set the course for the rest of the week. We used
  our Wednesday meetings as an opportunity to track and gauge our progress and also ask questions about
  difficult problems we came across during the previous week. We also talked about goals and milestones for the next
  Wednesday meeting and talked about any potential problems related to the difficulties of the goals we defined but also
  about any foreseeable road blocks related to tests, projects, other classes etc. that might hinder our progress. We used
  these meetings to make sure our project was progressing but also to shift our timeline to account for future roadblocks and delays.
  ii. Specifications
  We spent the first three weeks deciding the specifications of our language. We all met
  near a piano either in the basement of the dorms or in Lerner and went over intuitive ways
  for musicians to express music. Once we chose how we wanted to abstract notes, pitches, chords, tempos and more, we started
  talking about how to structure our language. We initially chose to do a functional language modelled after OCaml, but as we progressed,
  we realized that for the use cases we were targeting a fully functional language wouldn't give us the kind of ease of use and usage we'd like.
  Our first concrete specifications were the abstractions and then we decided on syntax. Despite having a very concrete definition of specifications
  early on, we still changed specifications as we worked on our language when it was necessary to be able to finish within our timeline.

  iii. Development
  Our team used github issues to define specifications and tasks that needed to be implemented or completed. We used github to help
  with organizing our development. Each of used a separate branch to develop the feature that we were working on and then submitted a pull
  request to the main branch once we thought it was ready. Then, another member of the team would review that request and merge the request. This ensured
  that all the code we pushed had been code reviewed and helped us maintain the quality of our mainline code.

  iv. Testing
  We developed a test suite that tested individual components of the compiler. Every time one of
  us was working on a small component, we first wrote a test for how that component was supposed to work
  once it was finished. When writing In this regard, we used some principles of Test-First Programming to make sure we were preserving
  the functionality of the older features but also ensuring functionality of the new ones. While some of these tests were forward
  looking and failed early-on, the error messages told the developer whether we were making progress towards making these larger full stack
  tests work or if it was failing in whatever module the developer was working on.
B. Programming Style Guide

1. Landin's pseudo law: Treat the indentation of your programs as if it determines the meaning of your programs.

2. Keep lines shorter than 80 characters.

3. Use line breaks for crazy pattern matching so the reader knows where's what.

4. Use two spaces instead of tabs when indenting.

5. Use underscores instead of Camel case when naming variables.

6. Avoid breaking expressions up into multiple lines unless absolutely necessary.

7. Comments go above the code they reference.

C. Timeline
Table
Date        Milestone
Jan 29th    First commit to repository
Feb 8th     Project Proposal and White Paper Completed and Submitted
Feb 22      Language Reference Manual Completed and Submitted
Mar 21      Basic Scanner Complete
Mar 26      Basic AST and Parser Complete
Mar 29      Hello World runs
Apr 7       Testing Framework Complete
Apr 23      Final Scanner, Parser and AST Complete
May 5       First Music Generated!
May 10      Project Completion

D. Roles and Responsibilities
While we had defined project roles at the beginning of the semester, about
three weeks in the roles became a lot more fluid. Our assigned roles were
Tester, System Architect, Project Manager and Language Guru. Each member was involved
in developing certain functionalities and portions of components. The team frequently worked
together either in-person or teleconference.
Team Member
Husam Abdul-Kafi  Systems Architect Testing Architecture, Code generation
Lakshmi Bodapati  Project Manager   Compiler Front-end, Documentation
Kaitlin Pet       Tester            Pitch and Chord Full Stack Abstraction, Testing Architecture
Harvey Wu         Language Guru     Compiler Front-end, Semantic Checker, Type Inference

E. Software Development Environment
Our Stack
/list
/l1 Version Control: Git / GitHub
/l2 Bug Repo: GitHub Issues
/l3 Language: Ocaml 4.04.0, C, Bash.
/l4 Editor: Vim (with Merlin plug-in), Sublime Text
/l5 Operating System: Ubuntu 16.04, Mac OSX 10.12

Testing Environment
/list
Google Cloud VM

F. Project Log
****************COPY FROM GIT WHEN FINISHED****************

Architectural Design Section
A. Block Diagram (http://www.cs.columbia.edu/~sedwards/classes/2012/w4115-fall/reports/Funk.pdf)                                            _______________ > Symbol Table
_______________________                                              |                       |
|Music-mike source code| -> Scanner -> Parser -> AST -> Type Inference outputs SAST -> Semantic Checking -> Code Generation -> LLVM code
________________________

B. Scanner (Lakshmi, Husam, Harvey)
Relevant Files: scanner.mll

The scanner is written in OCamlLex and takes the .mike input to the compiler
and tokenizes it into literals, identifiers, operators and keywords. It removes the white space
and block comments. If any character cannot be lexed by the scanner or if
any identifier or literal is not syntactically valid, the scanner throws an error. The tokens
created by the scanner are used by the Parser to create an Abstract Syntax Tree.

C. Parser (Husam, Harvey, Lakshmi, Kaitlin)
The parser is written in OCamlYacc and takes in a series of tokens. It uses the grammar described
in parser.mly and datatypes defined in ast.ml to generate an Abstract Syntax Tree. In parser.mly, we define
the Music-mike context-free grammar using productions. If the tokens produced by the scanner are successfully
parsed that means that the .mike file is syntactically correct.

D. Abstract Syntax Tree (Harvey, Husam)
Relevant Files: ast.ml

ast.ml houses two important data structures: program, which is a list of expressions and inferred_program, which is a list of type-annotated expressions. 

E. Semantic Checker ()
Relevant Files: infer.ml semant.ml

The semantic checker goes through a two-step process. The first is a modified version of Hindley-Milner type inference (Algorithm W) that takes the original AST as input (a list of expressions) and returns a list of type-inferred expressions.   
E. Code Generator (Husam)
F. C Libraries ()

Test Plan Section
A. *****************2 examples of source to code******************
B. Test Suites
We wrote tests every time we implemented a new feature. 
