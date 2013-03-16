//////////////////////////////////////////////////////
//
// Filename: $RCSfile: vec_node.c,v $
//
// Description:
//
// $Log: vec_node.c,v $
// Revision 1.1  2006/08/31 15:26:21  per47214c
// Added the new files
//
//////////////////////////////////////////////////////

// HEADERS //
#include <stdlib.h>
#include <strings.h>
#include "vec_node.h"

// MACROS//
#define FALSE 0
#define TRUE !(FALSE)

// MAIN CODE //
/* vec_node__init()
 *
 *
 *
 * Parameters:
 * Returns:
 */
struct vec_node* vec_node__init(char* d, struct vec_node* p, struct vec_node* n)
{
	// 
	struct vec_node* v = (struct vec_node*)malloc(sizeof(struct vec_node));
	if (v == NULL) return NULL;

	v->data = (char*)malloc(sizeof(char)*(strlen(d)+1));
	memset(v->data, '\0', strlen(d)+1);
	strncpy(v->data, d, strlen(d));
	//free(d);
	//v->data = d;
	v->prev = p;
	v->next = n;

	return v;
}

/* vec_node__destroy()
 *
 *
 *
 * Parameters:
 * Returns:
 */
int vec_node__destroy(struct vec_node* n)
{
	if (n == NULL) return (FALSE);

	// First deallocate the data
	if (n->data != NULL) {
		free(n->data);
		n->data = NULL;
	}
	
	// Deallocate the actual node
	free(n);

	return (TRUE);
}
