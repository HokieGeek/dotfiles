//////////////////////////////////////////////////////
//
// Filename: $RCSfile: queue.h,v $
//
// Description:
//
// $Log: queue.h,v $
// Revision 1.1  2006/08/31 15:26:21  per47214c
// Added the new files
//
//////////////////////////////////////////////////////

// HEADERS //
#include "vec.h"

#ifndef AFP_QUEUE_H
#define AFP_QUEUE_H

// 
typedef struct vec queue; 
typedef struct vec_node queue_node;

//
queue* queue_init();
int queue_destroy(queue* q);
int queue_enqueue(queue* q, char* data);
int queue_dequeue(queue* q);
char* queue_front(queue* q);
char* queue_at(queue* q, int pos);
int queue_empty(queue* q);
int queue_size(queue* q);

#endif
