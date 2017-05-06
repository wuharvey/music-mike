#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

int ***fold_lists ( int ***chord_list, int cl_length, int chord_lengths[],  int start_pitch, int *modelist, int mode_length){
printf("%s\n", "entering chord list");
//map the mode to absolute pitches (0 corresponds to first scale degree)
int i=0;
while (i<mode_length){
        modelist[i]=modelist[i]+start_pitch;
        printf("%s %d\n", "new mode value = ", modelist[i]);
	i+=1;
		}
//runs off assumption that malloced chunks are  not contiguous
int j=0;
while (j<cl_length){
	int ** chord= chord_list[j];
	int i=0;
	printf("%s %d\n", "chord number", j);
        while (i<chord_lengths[j]){
		printf("%s %d\n", "into while loop", i);
		printf("%p\n", chord);
		int * pitch= chord[i];
		printf("%s %d\n", "pitch number", i);
		printf("%s\n", "int *pitch=chord[i];");
		//new pitch to be added
		int transformed_pitch;
		int p=pitch[1];
		if (p==0){
			transformed_pitch=0;
		}
		else{
			int oup=(p-1)/mode_length;
			if (oup>0){
			//add 'overflow' to octaveup
			pitch[0]+=oup;
			}
			transformed_pitch=modelist[(p-1)%mode_length];
		}
		pitch[1]=transformed_pitch;

		i++;
		}
	j++;
	}

return chord_list;
}

//takes normalized chord list and spits out list with actual pitches
int ** apply_accidentals (int ***chordlist, int cl_len, int *chord_lengths, int mode_length, int **return_arr){
        int j=0;
        while (j<cl_len){
                int ** chord= chordlist[j];
                int i=0;
                while (i<chord_lengths[j]){
                        int * pitch= chord[i];
			if (pitch[1]!=0){
				//add or subtract octaves
				int octave_shift=pitch[0]*mode_length;
				pitch[1]=pitch[1]+octave_shift;
				//add or subtract accidentals
				pitch[1]=pitch[1]+pitch[2];
			}
			(return_arr[j])[i]=pitch[1];

			i++;
                        }
                j++;
                }


        return return_arr;
}


//string generator, takes rhythm list plus absolute pitch list and turns into strings that can be plopped in Cfugue
//gonna mix in some c++ lets see if it crashes :/
int strgen (char * buff, float * rhythmlist, int ** corrected_chordlist, int cl_len, int * chord_lengths){
	//make the buffer: max 11 characters per note (two digit pitch and 5 digit note length)

	int j=0;
	while (j<cl_len){
		//take note_len and convert into string
		double note_len=(double)rhythmlist[j];
		char snote_len[10];
		memset(snote_len, '\0', sizeof(snote_len));
		snprintf(snote_len, 10, "%.2f", note_len);
		//initialize chord
		int * chord= corrected_chordlist[j];
		int i=0;
		while (i<chord_lengths[j]){
			int pitch= chord[i];
			//convert pitch into string
			char pitchstring[3];
			memset(pitchstring, '\0', sizeof(pitchstring));
			snprintf(pitchstring, 3, "%d", pitch);
			//buffer all the goddamn symbols
			char lbracket[2];
			strcpy(lbracket, "[");
			char rbracket[3];
			strcpy(rbracket, "]\\");
			char plus[2];
			strcpy(plus, "+");
			char space[2];
			strcpy(space, " ");
                        strcat(buff, lbracket); strcat(buff, pitchstring); strcat(buff, rbracket); strcat(buff, snote_len);

			if (i<chord_lengths[j]-1){
				strcat(buff, plus);
				}
			else{
				strcat(buff, space);
				}
			i++;
			}
		j++;
		}

	return 0;


}



//synth- imitates behavior of main(), compared at end

int synth(int *** chordlist, int len_chordlist, int * chord_lengths, int start_pitch, int * modelist, int mode_length, float *rhythmlist, int **pure_chord_arr ){
	//modifies chordlist so mode is normalize to absolute value of notes. If range goes above octave, adds to prefield
	int ***new_list = fold_lists(chordlist, len_chordlist, chord_lengths, start_pitch, modelist, mode_length);
	//copies new_list into pure_chord_list to incorporate octaves and accidentals (yes, I know a new int ** is redundant but atm just want to see if works
	int **correct_pitches=apply_accidentals(new_list, len_chordlist, chord_lengths, mode_length, pure_chord_arr);

	//takes rhythm list and turns into string that can be fed into CFugue
        char buff[1000];//max length of each note should be 11 so more space than we need.
        buff[0]='\0';
        strgen (buff, rhythmlist, correct_pitches, len_chordlist, chord_lengths );
        execl("./testCFugueLib", "./testCFugueLib", buff);
	printf("%s\n", buff);

	return 0;
}





//tester
int main(){



	//variablesle we need
	int ***chordlist;
	int cl_len=4;
	int chord_lengths[4]={2,2,2,2};
	int start_pitch=10;
	int modelist[4]={1,3,5,7};
	int mode_length=4;
	float  rhythmlist[]={1, 1.5, 0.25, 0.33};
	//build chordlist
	chordlist=(int ***)malloc(4 * sizeof(int **));
	int r=0; //pitch literal value
	int c=-1; //accidental value
	int i=0; //number of chords
	while (i<4){
        	int **temp=(int **)malloc(2*sizeof(int *));
		printf("%s\n", "fuck pointers");
		chordlist[i]=temp;
		printf("%s\n", "pointers are cooeilo");
         	int j=0;
		while (j<2){
			printf("%s %d\n", "r = ", r);
			int* temp=(int *) malloc(3*sizeof(int));
			(chordlist[i])[j]=temp;
			int* pitch=(chordlist[i])[j];
			pitch[1]=r;
			pitch[2]=c;
			r++;
			j++;
		}
		i++;
	}


        printf("%s\n", "chord list was created");
	//
	int temp1[2];
	int temp2[2];
	int temp3[2];
	int temp4[2];
	int *modarr[4]={ temp1, temp2, temp3, temp4};

	//testing synth
        synth(chordlist, cl_len, chord_lengths, start_pitch,  modelist,  mode_length, rhythmlist, modarr );

	//let's see...
	int ***new_list = fold_lists(chordlist, cl_len, chord_lengths, start_pitch, modelist, mode_length);
	//print it!
	int j=0;
	while (j<cl_len){
		int ** chord= new_list[j];
		printf("%p\n", chord);
		int i=0;
		while (i<chord_lengths[j]){
			int * pitch= chord[i];
		        printf("\t%p\n", pitch);
		        printf("\t\t%d\n", pitch[0]);
			printf("\t\t%d\n", pitch[1]);
			printf("\t\t%d\n", pitch[2]);

			i++;
			}
		j++;
		}

	int **correct_pitches=apply_accidentals(new_list, cl_len, chord_lengths, mode_length, modarr);


        j=0;
        while (j<cl_len){
                int * chord= correct_pitches[j];
                printf("%p\n", chord);
                int i=0;
                while (i<chord_lengths[j]){
                        int pitch= chord[i];
                        printf("\t%d\n", pitch);
                        i++;
                        }
                j++;
                }


//testing adding rhythm to the whole shebang
	char buff[14* 8+1];
	buff[0]='\0';
	strgen (buff, rhythmlist, correct_pitches, cl_len, chord_lengths );
        printf("%s\n", buff);


//testing aggregate synth funtion

return 0;
}
