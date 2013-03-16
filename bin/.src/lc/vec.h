/////////////////////////////////////////////////////////////
// 
// Filename: $RCSfile: vec.h,v $
//
// Description:
//
// $Log: vec.h,v $
// Revision 1.1  2006/08/31 15:26:21  per47214c
// Added the new files
//
/////////////////////////////////////////////////////////////

#ifndef AFP_VEC_H
#define AFP_VEC_H

#include "vec_node.h"

struct vec {
	struct vec_node* head;
	struct vec_node* tail;
	struct vec_node* curr;
	int size;
	int pos;
};

// List Manipulators
struct vec* vec__list_init();
int vec__list_destroy(struct vec* list);
int vec__insert(struct vec* list, char* data);
int vec__delete(struct vec* list);
int vec__walk_head(struct vec* list);
int vec__walk_tail(struct vec* list);
int vec__walk(struct vec* list);
int vec__walk_r(struct vec* list);
char* vec__get_current(struct vec* list);
char* vec__at(struct vec* list, int pos);
int vec__empty(struct vec* list);
int vec__size(struct vec* list);

#endif
