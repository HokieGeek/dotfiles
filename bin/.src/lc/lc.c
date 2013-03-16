////////////////////////////////////////////////////////////
// 
// Filename: $RCSfile: lc.c,v $
// 
// Description: Displays a directory listing with type information and CVS information
// 
// $Log: lc.c,v $
// Revision 1.7  2006/08/31 15:28:13  per47214c
// - Made minor changes to organization
// - Removed uses of vector and string and replaced with new vector objects
//
// Revision 1.6  2006/08/09 20:43:55  per47214c
// - Fixed bug with cvs details always visible
// - Removed as many uses of string as possible
// - Renamed a couple of variables
// - Added code to read in cvs entries
// - Changed printFile
//
// Revision 1.5  2006/08/08 20:59:48  per47214c
// - Implemented detailed option with permissions, owner/group and filesize
// - Getting list of files in a directory with 'readdir' instead of calling 'ls'
// - Performs an lc on multiple files and directories passed as arguments
// - Implemented retrieving cvs info
//
// Revision 1.4  2006/08/07 20:53:45  per47214c
// - Implemented tarfile recognition
// - Implemented cvs info retrieval
// - Broke down big functions into smaller, for code reuse
// - Added function headers
//
////////////////////////////////////////////////////////////

//// Headers ////
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <dirent.h>
#include <grp.h>
#include <fcntl.h>
#include <errno.h>

#include "queue.h"

//// Type Definitions ////
#define MAGIC_NUM 256
#define MAX_DIR_PATH 2048
#define FALSE 0
#define TRUE !FALSE

typedef enum { EXE, PIPE, LINK, DIRECTORY, HID, ARCHIVE, OTHER } FILTERTYPE;
typedef struct _FileListsType {
	queue* Execs;
	queue* Pipes;
	queue* Links;
	queue* Archives;
	queue* Hidden;
	queue* Dirs;
	queue* Others;
} FileListsType;
typedef struct _OptionsType {
	int hasCVS;
	int recurse;
	int verbose;
	int detailed;
	int expandTar;
	char* level;
	char* All;
} OptionsType; 

//// Prototypes ////
void lc_file_(const char*, mode_t, OptionsType*, char*);
void lc_listing(char*, OptionsType*, char*);
int hasCVS(OptionsType*);

/* shell_cmd()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
char* shell_cmd(const char* cmd, int fdread, int fdclose)
{
	// Local Variables //
	int Len;
	char c;
	char* out;
	char Buff[51];
	int fd[2];
	Buff[50] = '\0';

	// 
	pipe(fd);
	if (fork() == 0)  {
		close(fdread);
		dup(fd[1]);
		close(fdclose);

		if (execlp("sh", "sh", "-c", cmd, 0))
			close(fd[1]);
	} else { //
		close(fd[1]);	
		out = (char*)malloc(sizeof(char)*255);
		out[0] = '\0';

		while((Len = read(fd[0], &c, 1)) > 0)
			if(!isspace((int)c)) sprintf(out, "%s%c", out, c);
	}

	return out;
}

/* shell_cmd_bool()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
int shell_cmd_bool(const char* cmd, int isBool)
{
	char* ret;
	ret = shell_cmd(cmd, 1, 2);

	if((ret == NULL) || (strlen(ret) <= 0) || ((atoi(ret) == 0) && (isBool))) 
		return (FALSE);
	return (TRUE); 
}

/* switcher()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
void switcher(char* sw, OptionsType* opt)
{
	// Local Variables //
	int i = 0;
	char* swtmp;

	// Error Handling: If either of the values are null, then exit
	if((sw == NULL) || (opt == NULL)) return;

	// 
	if(strlen(sw) > 1) {
		swtmp = (char*)malloc(sizeof(char)*2);
		swtmp[1] = '\0';
		for(i = 0; i < strlen(sw); i++) {
			strncpy(&swtmp[0], &sw[i], 1);
			switcher(swtmp, opt);
		}
	} else {
		switch(sw[0]) {
		case 'r': 
			strncat(opt->All, &sw[0], 1);
			opt->recurse = 1; 
			break;
		case 'v': 	
			strncat(opt->All, &sw[0], 1);
			opt->verbose = 1; 
			opt->detailed = 1;
			break;
		case 'd': 	
			strncat(opt->All, &sw[0], 1);
			opt->detailed = 1; 
			break;
		case 'x': 
			strncat(opt->All, &sw[0], 1);
			opt->expandTar = 1; 
			break;
		case 'h': /*TODO*/ break;
		default: break;
		}
	}
}

///////////////////////////////////////
// main()
//
// 
///////////////////////////////////////
int main(int argc, char *argv[])
{
	// Local Variables //
	int i = 0;
	int levelNum = 0;
	int filesNum = 0;
	int size = 0;
	char** files = NULL;
	char* Level = NULL;
	OptionsType Options;
	struct stat fs;

	// Argument Handler
	if(argc > 1) {

		// Switches data structure
		Options.All = (char*)malloc(sizeof(char)*MAGIC_NUM);
		//for(i = 0; i < MAGIC_NUM; i++) Options.All[i] = '\0';
		memset(Options.All, '\0', MAGIC_NUM);	
		Options.All[0] = '-';

		// Parse through the arguments
		for(i = 1; i < argc; i++) {
			if(!strcmp(argv[i], "-z")) { // A horribly "special" argument that states that lc is calling itself
				i++;
				levelNum = atoi(argv[i])*3;
				/*Options.level = (char*)malloc(sizeof(char)*levelNum);
				Options.level[levelNum] = '\0';
				for(levelNum--; levelNum >= 0; levelNum--)
					Options.level[levelNum] = ' ';*/
				Level = (char*)malloc(sizeof(char)*levelNum);
				Level[levelNum] = '\0';
				for(levelNum--; levelNum >= 0; levelNum--)
					Level[levelNum] = ' ';
				
			} else if((argv[i])[0] == '-') // Deal with any other option
				switcher(&(argv[i])[1], &Options);
			else {
				if(files == NULL) {
					files = (char**)malloc(sizeof(char)*MAGIC_NUM);
					files[0] = '\0';
				}

				size = strlen(argv[i]);
				files[filesNum] = (char*)malloc(sizeof(char)*(size+1));
				files[filesNum][size] = '\0';
				strncpy(files[filesNum], argv[i], size);
				filesNum++;
				files[filesNum] = '\0';
			}
		}
	}

	// Populate default level (none)
	/*if(Options.level == NULL) {
		Options.level = (char*)malloc(sizeof(char));
		Options.level[0] = '\0';
	}*/
	if(Level == NULL) {
		Level = (char*)malloc(sizeof(char));
		Level[0] = '\0';
	}

	Options.level = Level;

	// Traverse through list of files passed
	if(files != NULL) {
		for(i = 0; i < filesNum; i++) {
			if (lstat(files[i], &fs) == 0) {
				Options.hasCVS = hasCVS(&Options);
				if (S_ISDIR(fs.st_mode) && strcmp(files[i], "CVS")) {
					chdir(files[i]);
					lc_listing(getcwd(NULL, MAX_DIR_PATH+1), &Options, Level);
				} else
					lc_file_(files[i], fs.st_mode, &Options, Level);
			}
		}
	} else {
		Options.hasCVS = hasCVS(&Options);
		lc_listing(getcwd(NULL, MAX_DIR_PATH+1), &Options, Level);
	}
			

	// Cleanup
	if (files != NULL) free(files);//delete files;

	return 0;
}

// 						Random Helpers
//////////////////////////////////////////////////////////////

/* isTar()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
int isTar(const char* file)
{
	// Local Variables //
	char* tmp = NULL;

	if (file == NULL) return 0;

	tmp = (char*)strchr(file, '.');
	if (tmp != NULL) return !strncmp(tmp, ".tar", 4); 

	return 0;
}

/* isArchive()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
int isArchive(const char* file)
{ 
	return isTar(file); 
}

// 						CVS Information
///////////////////////////////////////////////////////////////

/* hasCVS()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
int hasCVS(OptionsType* opt)
{
	// Local Variables //
	DIR* dirp;
	struct dirent* entry;

	//
	dirp = opendir(getcwd(NULL, MAX_DIR_PATH+1));
	while((entry = readdir(dirp)) != NULL) {
		if(!strcmp(entry->d_name, "CVS")) {
			opt->hasCVS = 1;

			closedir(dirp);
			return (TRUE);
		}
	}
	closedir(dirp);

	return (FALSE);
} 

/* inCVS()
 *
 *
 *
 * Parameters:
 * Returns:
 */
int inCVS(const char* file)
{
	int ret = (FALSE);
	char* cmd  = (char*)malloc(sizeof(char)*MAGIC_NUM);

	sprintf(cmd, "grep '%s' CVS/Entries", file);
	ret = shell_cmd_bool(cmd, (FALSE));

	if (cmd != NULL) free(cmd);//delete cmd;
	return ret;
}

/* isModified_cvs()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
int isModified_cvs(const char* file)
{
	// Local Variables //
	int ret = 0;
	char* cmd = NULL;
	
	ret = (FALSE);
	cmd  = (char*)malloc(sizeof(char)*MAGIC_NUM);

	sprintf(cmd, "cvs diff %s 2>/dev/null | grep -c '^RCS'", file);
	ret = shell_cmd_bool(cmd, (TRUE));

	if (cmd != NULL) free(cmd);//delete cmd;
	return ret;
}

/* getCVSVerbose()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
char* getCVSVerbose()
{
	return "    CVS Verbose Information";
}

/* getCVSDetail()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
char* getCVSDetail(const char* file)
{
	// Local Variables //
	char* detail = NULL;
	char* cmd = NULL;

	if (file == NULL) return NULL;

	//
	cmd = (char*)malloc(sizeof(char)*MAGIC_NUM);
	detail = (char*)malloc(sizeof(char)*MAGIC_NUM);

	//
	// VERSION -> cvs status [file] | grep Working | cut -f2
	sprintf(cmd, "cvs status %s | grep Working | cut -f2 | head -1", file);
	sprintf(detail, "%s", shell_cmd(cmd, 1, 2));

	//
	// LINES -> cvs log -r lc.c | grep lines | cut -d':' -f7
	sprintf(cmd, "cvs log -r %s | grep lines | cut -d':' -f7 | head -1", file);
	sprintf(detail, "%s, %s", detail, shell_cmd(cmd, 1, 2));

	//
	// STATUS -> cvs status lc.c | grep Status | cut -f2 | cut -d':' -f2
	sprintf(cmd, "cvs status %s | grep Status | cut -f2 | cut -d':' -f2 | head -1", file);
	sprintf(detail, "%s, %s", detail, shell_cmd(cmd, 1, 2));

	//
	// STATE -> cvs log -r lc.c | grep state | cut -d' ' -f9
	sprintf(cmd, "cvs log -r %s | grep state | cut -d' ' -f9 | head -1", file);
	sprintf(detail, "%s, %s", detail, shell_cmd(cmd, 1, 2));

	if (cmd != NULL) free(cmd);//delete cmd;
	return detail;
}

// 						File Information
////////////////////////////////////////////////////////////////

/* getInfo()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
char getInfo(const char* file, OptionsType* opt)
{
	// Local Variables //
	//char info = ' ';

	// 
	if ((file == NULL) || (opt == NULL)) return ' ';
	if (!opt->hasCVS) return ' ';

	//
	if(inCVS(file)) {
		if (isModified_cvs(file))
			return '+';
		else
			return ' ';
	}
	return '-';
}

/* szFileSize()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
char* szFileSize(double size, int group)
{
	// Local Variables //
	double div = 0;
	double rem = 0;
	char* ret = NULL;
	char* unit = NULL;
	int nSize = size;

	//
	if (size >= 1024) {
		//div = int(size) / 1024;
		//rem = ((int(size) % 1024)/10)*0.01;
		div = nSize / 1024;
		rem = ((nSize % 1024)/10)*0.01;

		ret = szFileSize(div+rem, group+1);
	} else {
		unit = (char*)malloc(sizeof(char)*2);
		switch(group) {
		case 0: unit = "B"; break;
		case 1: unit = "KB"; break;
		case 2: unit = "MB"; break;
		case 3: unit = "GB"; break;
		case 4: unit = "TB"; break;
		case 5: unit = "PB"; break;
		}

		ret = (char*)malloc(sizeof(char)*20);
		sprintf(ret, "%6.2f %2s", size, unit);	
	} 

	// Cleanup
	//if (unit != NULL) free(unit);

	return ret;
}

/* getFilePerms()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
char* getFilePerms(const char* file, mode_t filemode)
{
	// Local Variables //
	char* perms = NULL;

	//
	if (file == NULL) return "---------";
	perms = (char*)malloc(sizeof(char)*10);
	perms[9] = '\0';

	// 
	perms[0] = filemode & S_IRUSR ? 'r' : '-';
	perms[1] = filemode & S_IWUSR ? 'w' : '-';
	perms[2] = filemode & S_IXUSR ? 'x' : '-';
	perms[3] = filemode & S_IRGRP ? 'r' : '-';
	perms[4] = filemode & S_IWGRP ? 'w' : '-';
	perms[5] = filemode & S_IXGRP ? 'x' : '-';
	perms[6] = filemode & S_IROTH ? 'r' : '-';
	perms[7] = filemode & S_IWOTH ? 'w' : '-';
	perms[8] = filemode & S_IXOTH ? 'x' : '-';

	return perms;
}

/* getFileDetail()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
/*char* getFileDetail(const char* file)
{
	// Local Variables //
	struct stat fs;
	char* ret = NULL;
	char* details = NULL;
	
	if (file == NULL) return " ";

	if (lstat(file, &fs) == 0) {
		details = (char*)malloc(sizeof(char)*MAGIC_NUM);
		memset(details, '\0', MAGIC_NUM);
		
		sprintf(details, "%i:%s", (int)fs.st_uid, getgrgid(fs.st_gid)->gr_name);

		ret = (char*)malloc(sizeof(char)*(9+strlen(details)+8+1));

		sprintf(ret, "%s %s %s", getFilePerms(file, fs.st_mode), 
					 details, szFileSize(fs.st_size, 0));
	}

	if (details != NULL) free(details);//delete details;
	return ret;
}*/

// 						File Printers
/////////////////////////////////////////////////////////////

/* printFile()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
void printFile(const char* file, char* name, OptionsType* opt, char* level)
{
	//if (opt->detailed == 1) printf("[%s]   ", getFileDetail(file));
	//printf("%s%c%s", opt->level, getInfo(file, opt), name);
	printf("%s%c%s", level, getInfo(file, opt), name);

	//if (opt->detailed == 1 && inCVS(file)) printf("  (%s)", getCVSDetail(file));
	if ((1) && (opt->detailed == 1 && inCVS(file)))
		printf("  (%s)", getCVSDetail(file));
	printf("\n");
	//if (opt->verbose == 1 && opt->hasCVS == 1) printf("%s\n", getCVSVerbose());
}
		
/* lc_dir()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
void lc_dir(const char* dir, OptionsType* opt, char* level)
{
	// Local Variables //
	char* name = NULL;
	int newLevelSize = 0;
	char* newLevel = NULL;
	char* newDir = NULL;

	//int Len;
	//char Buff[51];
	//Buff[50] = '\0';

	//
	if ((dir == NULL) || (opt == NULL)) return;
	name = (char*)malloc(sizeof(char)*(strlen(dir)+2));
	memset(name, '\0', strlen(dir)+2);
	strncpy(name, dir, strlen(dir));
	strncat(name, "/", 1);
	
	printFile(dir, name, opt, level);

	if(opt->recurse == 1) {
		newLevelSize = sizeof(char)*strlen(level)+3;
		newLevel = (char*)malloc(newLevelSize);
		memset(newLevel, '\0', strlen(newLevel)+1);
		newLevel[newLevelSize] = '\0';

		for(newLevelSize--; newLevelSize >= 0; newLevelSize--)
			newLevel[newLevelSize] = ' ';

		// 
		newDir = (char*)malloc(MAX_DIR_PATH+1);
		strncpy(newDir, getcwd(NULL, MAX_DIR_PATH+1), MAX_DIR_PATH+1);
		strncat(newDir, "/", 1);

		chdir(newDir);
		lc_listing(newDir, opt, newLevel);
		chdir("..");
	
		if (newDir != NULL) free(newDir);
		if (newLevel != NULL) free(newLevel);
	}
}

/* lc_link()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
void lc_link(const char* link, OptionsType* opt, char* level)
{
	// Local Variables //
	char* target = NULL;
	char* name = NULL;

	//
	if((link == NULL) || (opt == NULL)) return;

	target = (char*)malloc(sizeof(char)*MAGIC_NUM);
	memset(target, '\0', MAGIC_NUM);

	// 
	if (readlink(link, target, MAGIC_NUM) != -1) {
		name = (char*)malloc(sizeof(char)*(strlen(link)+strlen(target)+5));
		sprintf(name, "%s -> %s", link, target);
	} else {
		name = (char*)malloc(sizeof(char)*(strlen(link)+2));
		strncpy(name, link, strlen(link));
		strncat(name, "@", 1);
	}
	printFile(link, name, opt, level);

	// Cleanup
	if (target != NULL) free(target);//delete target;
}

/* lc_archive()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
void lc_archive(const char* ar, OptionsType* opt, char* level)
{
	// Local Variables //
	char* name = NULL;

	//
	if ((ar == NULL) || (opt == NULL)) return;
	name = (char*)malloc(sizeof(char)*(strlen(ar)+2));
	memset(name, '\0', strlen(ar)+2);
	//name[strlen(ar)+2] = '\0';
	strncpy(name, ar, strlen(ar));
	strncat(name, "#", 1);

	//
	printFile(ar, name, opt, level);
}

/* lc_other()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
void lc_other(const char* file, FILTERTYPE type, OptionsType* opt, char* level)
{
	// Local Variables //
	char* name = NULL;

	//
	if ((file == NULL) || (opt == NULL)) return;
	name = (char*)malloc(sizeof(char)*(strlen(file)+2));
	memset(name, '\0', strlen(file)+2);
	//name[strlen(file)+1] = '\0';
	strncpy(name, file, strlen(file));

	//
	switch(type) {
	case EXE: strncat(name, "*", 1); break;
	case PIPE: strncat(name, "|", 1); break;
	default: break;
	}
	printFile(file, name, opt, level);

	//
	if (name != NULL) free(name);//delete name;
}

// 						File parsers
////////////////////////////////////////////////////////////

/* lc_file()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
void lc_file(const char* file, FILTERTYPE type, OptionsType* opt, char* level)
{
	switch(type) {
	case LINK: 	lc_link(file, opt, level); break;
	case DIRECTORY: lc_dir(file, opt, level); break;
	case ARCHIVE: 	lc_archive(file, opt, level); break;
	default: 	lc_other(file, type, opt, level); break;
	}
}

/* lc_file()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
void lc_file_(const char* file, mode_t mode, OptionsType* opt, char* level)
{
	FILTERTYPE type = OTHER;

	if (S_ISLNK(mode)) 		  type = LINK;
	else if (S_ISDIR(mode)) 	  type = DIRECTORY;
	else if (isArchive(file)) 	  type = ARCHIVE;
	else if (S_ISFIFO(mode)) 	  type = PIPE;
	else if (access(file, X_OK) == 0) type = EXE;

	lc_file(file, type, opt, level);
}

/* lc_files()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
void lc_files(queue* files, FILTERTYPE type, OptionsType* opt, char* level)
{
	/*while(!queue_empty(files)) {
		lc_file(queue_front(files), type, opt);
		if(!queue_dequeue(files))
			printf("ERROR: cannot dequeue file\n");
	}*/
	// Local Variables //
	int i = -1;
	
	for(i = 0; i < queue_size(files); i++)
		lc_file(queue_at(files, i), type, opt, level);
}

/* populateLists()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
//void populateLists(FileListsType* files, char* dir, OptionsType* opt, char* level)
void populateLists(FileListsType* files, char* dir, OptionsType* opt)
{
	// Local Variables //
	struct stat fs;
	DIR* dirp;
	struct dirent* entry;
	//printf("Looking at: %s\n",dir);
	//
	dirp = opendir(dir);
	if (dirp == NULL) perror(dir);

	while((dirp != NULL) && ((entry = readdir(dirp)) != NULL)) {
		if (strcmp(entry->d_name, ".") && strcmp(entry->d_name, "..") && strcmp(entry->d_name, "CVS")) {
			if (entry->d_name[0] != '.') {
				/*if (lstat(entry->d_name, &fs) == 0) {
					if (S_ISLNK(fs.st_mode))
						lc_link(entry->d_name, opt, level);
					else if (S_ISFIFO(fs.st_mode))
						lc_other(entry->d_name, PIPE, opt, level);
					else if (S_ISDIR(fs.st_mode))
						queue_enqueue(files->Dirs, entry->d_name);
					else if (isArchive(entry->d_name))
						lc_archive(entry->d_name, opt, level);
					else if (access(entry->d_name, X_OK) == 0)
						lc_other(entry->d_name, EXE, opt, level);
					else
						lc_other(entry->d_name, OTHER, opt, level);
				}*/
				if (lstat(entry->d_name, &fs) == 0) {
					if (S_ISLNK(fs.st_mode))
						queue_enqueue(files->Links, entry->d_name);
					else if (S_ISFIFO(fs.st_mode))
						queue_enqueue(files->Pipes, entry->d_name);
					else if (S_ISDIR(fs.st_mode))
						queue_enqueue(files->Dirs, entry->d_name);
					else if (isArchive(entry->d_name))
						queue_enqueue(files->Archives, entry->d_name);
					else if (access(entry->d_name, X_OK) == 0)
						queue_enqueue(files->Execs, entry->d_name);
					else
						queue_enqueue(files->Others, entry->d_name);
				} 
			}
		}
	}
	
	if (dirp != NULL) closedir(dirp);
}

/* lc_listing()
 *
 *
 *
 * Parameters: 
 * Returns: 
 */
void lc_listing(char* file, OptionsType* options, char* level)
{
	// Local Variables //
	FileListsType* files = NULL;

	// Error Handling
	if ((file == NULL) || (options == NULL)) return;

	// Create the lists
	files = (FileListsType*)malloc(sizeof(FileListsType));
	files->Execs = queue_init();
	files->Pipes = queue_init();
	files->Links = queue_init();
	files->Archives = queue_init();
	files->Hidden = queue_init();
	files->Dirs = queue_init();
	files->Others = queue_init();

	//populateLists(files, file, options, level);
	populateLists(files, file, options);

	//
	if (!queue_empty(files->Execs)) 	lc_files(files->Execs, EXE, options, level);
	if (!queue_empty(files->Pipes)) 	lc_files(files->Pipes, PIPE, options, level);
	if (!queue_empty(files->Others)) 	lc_files(files->Others, OTHER, options, level);
	if (!queue_empty(files->Links)) 	lc_files(files->Links, LINK, options, level);
	if (!queue_empty(files->Archives)) 	lc_files(files->Archives, ARCHIVE, options, level);
	if (!queue_empty(files->Dirs)) 		lc_files(files->Dirs, DIRECTORY, options, level);

	// Cleanup
	/*if (!queue_destroy(files->Execs)) 	printf("ERROR: cannot destroy list\n");
	if (!queue_destroy(files->Pipes)) 	printf("ERROR: cannot destroy list\n");
	if (!queue_destroy(files->Links)) 	printf("ERROR: cannot destroy list\n");
	if (!queue_destroy(files->Archives)) 	printf("ERROR: cannot destroy list\n");
	if (!queue_destroy(files->Hidden)) 	printf("ERROR: cannot destroy list\n");
	if (!queue_destroy(files->Dirs)) 	printf("ERROR: cannot destroy list\n");
	if (!queue_destroy(files->Others)) 	printf("ERROR: cannot destroy list\n");
	if (files != NULL) free(files);*/
}
