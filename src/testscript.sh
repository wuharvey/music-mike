
basename=`echo $1 | sed 's/.*\\///
                             s/.mike//'`


rm *.ll *.s *.exe *.out

./musicmike.native < "${basename}.mike" >  "${basename}.ll"

llc "${basename}.ll" > "${basename}.s"

cc -o "${basename}.exe" "${basename}.s" "synth.o"  "make_midi.o"

"./${basename}.exe" > "${basename}.out"


echo "############LLVM############"


cat "${basename}.ll"


echo "############OUTPUT############"

cat "${basename}.out"
