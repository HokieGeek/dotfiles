/********************************************************************
 *                                                                  *
 * Filename: psenv.c                                                *
 *                                                                  *
 * Description: Outputs the environment of the specified process    *
 *								    * 
 * @author	per47214c					    *	
 *                                                                  *
 * Copyright (c) 2006 JDSU                                          *
 *                                                                  *
 ********************************************************************/

/* Includes */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>
#include <fcntl.h>
#include <procfs.h>
#include <sys/types.h>
#include <dirent.h>

/* print_psenv()
 *
 * @param 	pid 	The pid of the selected process
 * @author	per47214c
 */
int print_psenv(pid_t pid)
{
   /* Local Variables */
   char fname[PATH_MAX+1];
   int fd = -1;
   psinfo_t* data = NULL;
   long maxsize = 0;
   off_t envloc = 0;
   char* envp = NULL;
   char* buf = NULL;

   sprintf(fname, "/proc/%d/psinfo", pid); /* Construct psinfo path from pid */

   /* Open up the pid's psinfo file */
   if((fd = open(fname, O_RDONLY)) == -1)
   {
      /*fprintf(stderr, "Could not open psinfo file for pid %d\n", pid);*/
      return(-1);
   }

   /* read in the psinfo structure */
   data = (psinfo_t *)malloc(sizeof(psinfo_t));
   if(read(fd, data, sizeof(psinfo_t)) == -1)
   {
      /*fprintf(stderr, "Failed read of psinfo\n");*/
      return(-1);
   }
   close(fd);

   sprintf(fname, "/proc/%d/as", pid); /* Construct image path from pid */

   /* Open up the pid's image file */
   if((fd = open(fname, O_RDONLY)) == -1)
   {
      /*fprintf(stderr, "Failed read image\n");*/
      return(-1);
   }

   /* if the envp vector pointer exists, get the vector */
   if(data->pr_envp)
   {
      if(pread(fd, &envp, sizeof(char **), data->pr_envp) == -1)
      {
         /*fprintf(stderr, "Failed to get initial env list vector for pid %d\n", pid);*/
         return(-1);
      } 

      /* Initialize variables */
      maxsize = sysconf(_SC_ARG_MAX);
      envloc = data->pr_envp;
      buf = (char*)malloc(maxsize);

      /* Read all environment variables from the image */
      do {
          if (pread(fd, buf, maxsize-1,(off_t)envp) <= 0)
	  {
	    /*fprintf(stderr, "%s\n", "Cannot read process image");*/
	    return(-1);
	  }
	  else
	  {
	    printf("%s\n", buf); /* This is where we print each environment variable we find */

	    envloc += sizeof(char *);

	    /* Determine pointer to next variable */
	    if (pread(fd, &envp, sizeof(char *),envloc) <= 0)
	    {
		/*fprintf(stderr, "%s\n", "Cannot read process image");*/
	        return(-1);
	    }
	  }
      } while (envp != NULL);
   }
   else
   {
      /*fprintf(stderr, "%s\n", "No environment pointer in structure");*/
      return(-1);
   }

   /* Housekeeping */
   close(fd);
   if (fname != NULL) free(fname);
   if (buf != NULL) free(buf);
   if (data != NULL) free(data);
   if (envp != NULL) free(envp);

   return 0;
}

/* main()
 *
 * @param 	argc 	The number of arguments found
 *		argv 	Array with arguments passed to the program
 * @author	per47214c
 */
int main(int argc, char **argv)
{
   /* Local Variables */
   int pid = -1;
   long maxsize = sysconf(_SC_ARG_MAX);
   char* buf = (char*)malloc(maxsize);
	
   /* Argument Handler */
   if(argc < 2)
   {
        if(read(0, buf, maxsize) > 0)
        {
	     pid = atoi(buf);
        } 
   } 
   else
   {
        if (!strcmp(argv[1], "-?")) 
        {
    	     fprintf(stderr, "Usage: %s [PID]\n", argv[0]);
	     return 0;
        }
        else
        {
             pid = atoi(argv[1]);
        }
    }
   
   /* Print the environment */
   if (print_psenv(pid) != 0)
   {
      /* Process has ended? */
      fprintf(stderr, "Could not obtain process info for pid %d\n", pid);
   }

   return 0;
}
