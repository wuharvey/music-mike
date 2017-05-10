#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>


int ***fold_lists ( int ***chord_list, int cl_length, int chord_lengths[],
 int start_pitch, int *modelist, int mode_length){
fprintf(stderr,"%s\n", "entering chord list");
//map the mode to absolute pitches (0 corresponds to first scale degree)
int i=0;
while (i<mode_length){
        modelist[i]=modelist[i]+start_pitch;
        fprintf(stderr,"%s %d\n", "new mode value = ", modelist[i]);
	i+=1;
		}
//runs off assumption that malloced chunks are  not contiguous
int j=0;
while (j<cl_length){
	int ** chord= chord_list[j];
	int i=0;
	fprintf(stderr,"%s %d\n", "chord number", j);
        while (i<chord_lengths[j]){
		fprintf(stderr,"%s %d\n", "into while loop", i);
		fprintf(stderr,"%p\n", chord);
		int * pitch= chord[i];
		fprintf(stderr,"%s %d\n", "pitch number", i);
		fprintf(stderr,"%s\n", "int *pitch=chord[i];");
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
fprintf(stderr,"%s\n", "after while loop");
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
int strgen (char * buff, double * rhythmlist, int ** corrected_chordlist, int cl_len, int * chord_lengths, int channel){
       //first add channel to beginning
	char v[3];
	strcpy(v, "V");
	strcat(buff, v);
	char channel_buff[3];
	snprintf(channel_buff, 3, "%d", channel);
	strcat(buff, channel_buff);
	char space[2];
	strcpy(space, " ");
	strcat(buff, space);
	int j=0;
	while (j<cl_len){
		//take note_len and convert into string
		double note_len= rhythmlist[j];
		fprintf(stderr, "note_len: %.2f\n", note_len);
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
			snprintf(pitchstring, 3, "%d", pitch);
			//buffer all the goddamn symbols
			char lbracket[2];
			strcpy(lbracket, "[");
			char rbracket[3];
			strcpy(rbracket, "]/");
			char plus[2];
			strcpy(plus, "+ ");
			char space[2];
			strcpy(space, " ");
			char rest[3];
			strcpy(rest, "R/");
			//make acutal pitchstring
			if (pitch==0){
				strcat(buff, rest);
			}
			else{
				strcat(buff, lbracket);
				strcat(buff, pitchstring);
				strcat(buff, rbracket);
			}
                        strcat(buff, snote_len);

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

int synth(int *** chordlist, int len_chordlist, int * chord_lengths, 
	int start_pitch, int * modelist, int mode_length, double *rhythmlist,
	int **pure_chord_arr, int channel, char * buff){
	fprintf(stderr,"%s\n", "in synth");

	int *** new_chordlist = (int ***) malloc(len_chordlist * sizeof(int **));

	int j1=0;
	while (j1<len_chordlist){
		fprintf(stderr, "%d\n", j1);
		fprintf(stderr, "%s\n", "LINE 162");
		int ** chord= chordlist[j1]; //old
		int ** new_chord = (int **) malloc(chord_lengths[j1]*sizeof(int *));		
		new_chordlist[j1]=new_chord;//stuff in
		int i=0;
		while (i<chord_lengths[j1]){
			fprintf(stderr, "%s\n", "LINE 167");
			int *pitch= chord[i];//old
			int *new_pitch = (int *) malloc(3*sizeof(int));
			new_chord[i]=new_pitch;
			new_pitch[0] = pitch[0];
			new_pitch[1] = pitch[1];
			new_pitch[2] = pitch[2];
			fprintf(stderr, "%s\n", "LINE 173");
			fprintf(stderr, "%s\n", "LINE 175");
			i++;
			}	
		j1++;
	}

	int *new_modelist = (int *) malloc(mode_length * sizeof(int *));
	int j2 = 0;
	while(j2 < mode_length) {
		new_modelist[j2] = modelist[j2];
		j2++;
		}

	// int i = 0;
	// while (i<len_chordlist){
 //        int **temp=(int **)malloc(chord_lengths[i]*sizeof(int *));
	// 	fprintf(stderr,"%s\n", "fuck pointers");
	// 	new_chordlist[i]=temp;
	// 	fprintf(stderr,"%s\n", "pointers are cooeilo");
 //        int j=0;
	// 	while (j<chord_lengths[i]){
	// 		fprintf(stderr,"%s %d\n", "r = ", r);
	// 		int* temp2=(int *) malloc(3*sizeof(int));
	// 		temp[j] = temp2;
	// 		// (chordlist[i])[j]=temp2;
	// 		int* pitch=(chordlist[i])[j];
	// 		pitch[1]=r;
	// 		pitch[2]=c;
	// 		r++;
	// 		j++;
	// 	}
	// 	i++;
	// }



	// int j=0;
	// while (j<len_chordlist){
	// 	int ** chord= chordlist[j];
	// 	fprintf(stderr,"%p\n", chord);
	// 	int i=0;
	// 	while (i<chord_lengths[j]){
	// 		int * pitch= chord[i];
	// 	        fprintf(stderr,"\t%p\n", pitch);
	// 	        fprintf(stderr,"\t\t%d\n", pitch[0]);
	// 		fprintf(stderr,"\t\t%d\n", pitch[1]);
	// 		fprintf(stderr,"\t\t%d\n", pitch[2]);

	// 		i++;
	// 		}
	// 	j++;
	// 	}
	//modifies chordlist so mode is normalize to absolute value of notes. If range goes above octave, adds to prefield
	int ***new_list = fold_lists(new_chordlist, len_chordlist, chord_lengths, start_pitch, new_modelist, mode_length);
	fprintf(stderr,"%s\n", "AFTER NEW LIST");
	int j=0;
	while (j<len_chordlist){
		int ** chord= new_list[j];
		fprintf(stderr,"%p\n", chord);
		int i=0;
		while (i<chord_lengths[j]){
			int * pitch= chord[i];
		        fprintf(stderr,"\t%p\n", pitch);
		        fprintf(stderr,"\t\t%d\n", pitch[0]);
			fprintf(stderr,"\t\t%d\n", pitch[1]);
			fprintf(stderr,"\t\t%d\n", pitch[2]);

			i++;
			}
		j++;
		}
	//copies new_list into pure_chord_list to incorporate octaves and accidentals (yes, I know a new int ** is redundant but atm just want to see if works
	fprintf(stderr,"%s\n", "after new_list");
	int **correct_pitches=apply_accidentals(new_list, len_chordlist, chord_lengths, mode_length, pure_chord_arr);
	fprintf(stderr,"%d\n", chord_lengths[0]);
	//takes rhythm list and turns into string that can be fed into CFugue
        memset(buff, '\0', 900);
        strgen (buff, rhythmlist, correct_pitches, len_chordlist, chord_lengths, channel );
		fprintf(stderr,"buff %s\n", buff);

	return 0;
}





// //tester
   // int main(){



// 	//variablesle we need
// 	int ***chordlist;
// 	int cl_len=4;
// 	int chord_lengths[4]={2,2,2,2};
// 	int start_pitch=10;
// 	int modelist[4]={1,3,5,7};
// 	int mode_length=4;
// 	double  rhythmlist[]={1, 1.5, 0.25, 0.33};
// 	//build chordlist
// 	chordlist=(int ***)malloc(4 * sizeof(int **));
// 	int r=0; //pitch literal value
// 	int c=-1; //accidental value
// 	int i=0; //number of chords
// 	while (i<4){
//         	int **temp=(int **)malloc(2*sizeof(int *));
// 		fprintf(stderr,"%s\n", "fuck pointers");
// 		chordlist[i]=temp;
// 		fprintf(stderr,"%s\n", "pointers are cooeilo");
//          	int j=0;
// 		while (j<2){
// 			fprintf(stderr,"%s %d\n", "r = ", r);
// 			int* temp=(int *) malloc(3*sizeof(int));
// 			(chordlist[i])[j]=temp;
// 			int* pitch=(chordlist[i])[j];
// 			pitch[1]=r;
// 			pitch[2]=c;
// 			r++;
// 			j++;
// 		}
// 		i++;
// 	}


//         fprintf(stderr,"%s\n", "chord list was created");
// 	//
// 	int temp1[2];
// 	int temp2[2];
// 	int temp3[2];
// 	int temp4[2];
// 	int *modarr[4]={ temp1, temp2, temp3, temp4};

// 	//testing synth
//         synth(chordlist, cl_len, chord_lengths, start_pitch,  modelist,  mode_length, rhythmlist, modarr );

// 	//let's see...
// 	int ***new_list = fold_lists(chordlist, cl_len, chord_lengths, start_pitch, modelist, mode_length);
// 	//print it!
// 	int j=0;
// 	while (j<cl_len){
// 		int ** chord= new_list[j];
// 		fprintf(stderr,"%p\n", chord);
// 		int i=0;
// 		while (i<chord_lengths[j]){
// 			int * pitch= chord[i];
// 		        fprintf(stderr,"\t%p\n", pitch);
// 		        fprintf(stderr,"\t\t%d\n", pitch[0]);
// 			fprintf(stderr,"\t\t%d\n", pitch[1]);
// 			fprintf(stderr,"\t\t%d\n", pitch[2]);

// 			i++;
// 			}
// 		j++;
// 		}

// 	int **correct_pitches=apply_accidentals(new_list, cl_len, chord_lengths, mode_length, modarr);


//         j=0;
//         while (j<cl_len){
//                 int * chord= correct_pitches[j];
//                 fprintf(stderr,"%p\n", chord);
//                 int i=0;
//                 while (i<chord_lengths[j]){
//                         int pitch= chord[i];
//                         fprintf(stderr,"\t%d\n", pitch);
//                         i++;
//                         }
//                 j++;
//                 }


// //testing adding rhythm to the whole shebang
// 	char buff[14* 8+1];
// 	buff[0]='\0';
// 	strgen (buff, rhythmlist, correct_pitches, cl_len, chord_lengths );
//         fprintf(stderr,"%s\n", buff);


// //testing aggregate synth funtion

// return 0;
// }
