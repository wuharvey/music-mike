#!/bin/sh

echo "Enter base\n"
read base

rm *.ll *.s *.exe *.out

echo "./src/musicmike.native < ./tests/${base}.mike >  ${base}.ll"
./src/musicmike.native < ./tests/${base}.mike >  ${base}.ll

echo "llc -relocation-model=pic ${base}.ll > ${base}.s"
llc -relocation-model=pic "${base}.ll" ">" "${base}.s"

echo " cc -o ${base}.exe ${base}.s ./src/synth.o ./src/make_midi.o"
cc -o ${base}.exe ${base}.s ./src/synth.o ./src/make_midi.o

echo "./${base}.exe > ${base}.out"
./${base}.exe > ${base}.out

echo "############LLVM############"
cat ${base}.ll

echo "############OUTPUT############"
cat ${base}.out

