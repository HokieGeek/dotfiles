////////////////////////////////////////////////////////////
// 
// Filename: $RCSfile: mkhr.c,v $
// 
// Description: 
// 
// $Log$
//
////////////////////////////////////////////////////////////

//// Headers ////
#include <stdio.h>
#include <strings.h>
#include <ctype.h>

//// Type Definitions ////
#define FALSE 0
#define TRUE !FALSE

/* switcher()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
void switcher(char* sw, int* from, int* to)
{
	// Local Variables //
	int i = 0;
	char* swtmp;

	// Error Handling: If either of the values are null, then exit
	if (sw == NULL) return;

	// 
	if (strlen(sw) > 2) {
		swtmp = (char*)malloc(sizeof(char)*2);
		swtmp[1] = '\0';
		for(i = 0; i < strlen(sw); i++) {
			strncpy(&swtmp[0], &sw[i], 1);
			switcher(swtmp, from, to);
		}
	} else {
		switch(sw[0]) {
		case 't': 
			switch(sw[1]) {
			case 'k': *to = 1; break;	
			case 'm': *to = 2; break;
			case 'g': *to = 3; break;
			case 't': *to = 4; break;
			case 'p': *to = 5; break;
			case 'b': 
			default: *to = 0; break;
			}
			break;
		case 'f': break;
		}
	}
}

/* makeReadable()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
char* makeReadable(double size, int u, int displayUnit)
{
	// Local Variables //
	double div = 0;
	double rem = 0;
	char* ret = NULL;
	char* unit = NULL;
	int nSize = size;

	//
	if ((u < 8) && (size >= 1024)) {
		div = nSize / 1024;
		rem = ((nSize % 1024)/10)*0.01;

		ret = makeReadable(div+rem, u+1, displayUnit);
	} else {
		char* unit = (char*)malloc(sizeof(char)*2);

		switch(u) {
		case 0: unit = "B"; break;  // Bytes
		case 1: unit = "kB"; break; // Kilobytes
		case 2: unit = "MB"; break; // Megabytes
		case 3: unit = "GB"; break; // Gigabytes
		case 4: unit = "TB"; break; // Terabytes
		case 5: unit = "PB"; break; // Petabytes
		case 6: unit = "EB"; break; // Exabytes
		case 7: unit = "ZB"; break; // Zettabytes
		case 8: unit = "YB"; break; // Yottabytes
		}

		if (displayUnit) {
			ret = (char*)malloc(sizeof(char)*20);
			sprintf(ret, "%6.2f %2s", size, unit);
		} else {
			ret = (char*)malloc(sizeof(char)*18);
			sprintf(ret, "%6.2f", size);
		}
	} 

	return ret;
}

/* lowercase()
 *
 *
 * Parameters:
 * Returns:
 */
char* lowercase(char* str)
{
	int i = -1;
	char* lower = (char*)malloc(sizeof(char)*(strlen(str)+1));
	memset(lower, '\0', strlen(str)+1);

	for(i = 0; i < strlen(str); i++)
		lower[i] = tolower(str[i]); 
	return lower;
}

///////////////////////////////////////
// main()
//
// 
///////////////////////////////////////
int main(int argc, char *argv[])
{
	// Local Variables //
	double size = 0;
	int from = 0;
	char* unit = NULL;

	// Argument Handler
	if(argc > 1) {
		if (!strcmp(argv[1], "-?") || 
		    !strcmp(argv[1], "--?") ||
		    !strcmp(argv[1], "-help") || 
		    !strcmp(argv[1], "--help")) {
			printf("Usage: mkhr NUMBER [UNIT]\n");
			return 0;
		} else {
			size = atoi(argv[1]);

			if (argc > 2) {
				switch(tolower((argv[2])[0])) {
				case 'k': from = 1; break; // Kilobytes
				case 'm': from = 2; break; // Megabytes
				case 'g': from = 3; break; // Gigabytes
				case 't': from = 4; break; // Terabytes
				case 'p': from = 5; break; // Petabytes
				case 'e': from = 6; break; // Exabytes
				case 'z': from = 7; break; // Zettabytes
				case 'y': from = 8; break; // Yottabytes
				default: from = 0; break; // Bytes
				}
			}
		}
	}

	printf("%s\n", makeReadable(size, from, (TRUE)));

	return 0;
}
