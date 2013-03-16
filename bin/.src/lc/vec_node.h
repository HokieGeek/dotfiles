////////////////////////////////////////////////////
//
// Filename: $RCSfile: vec_node.h,v $
//
// Description:
//
// $Log: vec_node.h,v $
// Revision 1.1  2006/08/31 15:26:22  per47214c
// Added the new files
//
////////////////////////////////////////////////////

#ifndef AFP_VEC_NODE_H
#define AFP_VEC_NODE_H

struct vec_node;

struct vec_node {
	struct vec_node* prev;
	struct vec_node* next;
	char* data;
};

// Node Manipulators
//struct vec_node* vec_node__init(char* d, struct vec_node* p = NULL, struct vec_node* n = NULL);
struct vec_node* vec_node__init(char* d, struct vec_node* p, struct vec_node* n);
int vec_node__destroy(struct vec_node* n);

#endif
