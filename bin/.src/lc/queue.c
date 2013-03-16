//////////////////////////////////////////////////////
//
// Filename: $RCSfile: queue.c,v $
//
// Description:
//
// $Log: queue.c,v $
// Revision 1.1  2006/08/31 15:26:21  per47214c
// Added the new files
//
//////////////////////////////////////////////////////

// HEADER //
#include "queue.h"

// MAIN CODE //
/* queue_init()
 *
 *
 * Parameters:
 * Returns:
 */
queue* queue_init()
{
	return vec__list_init();
}

/* queue_destroy()
 *
 *
 * Parameters:
 * Returns:
 */
int queue_destroy(queue* q)
{
	return vec__list_destroy(q);
}

/* queue_enqueue()
 *
 *
 * Parameters:
 * Returns:
 */
int queue_enqueue(queue* q, char* data)
{
	int ret = -1;
	vec__walk_tail(q);
	ret = vec__insert(q, data);

	return ret;
	//return vec__insert(q, data);
}

/* queue_dequeue()
 *
 *
 * Parameters:
 * Returns:
 */
int queue_dequeue(queue* q)
{
	vec__walk_head(q);	
	return vec__delete(q);
}

/* queue_front()
 *
 *
 * Parameters:
 * Returns:
 */
char* queue_front(queue* q)
{
	vec__walk_head(q);
	return vec__get_current(q);
}

/* queue_at()
 *
 *
 * Parameters:
 * Returns:
 */
char* queue_at(queue* q, int pos)
{
	return vec__at(q, pos);
}

/* queue_empty()
 *
 *
 * Parameters:
 * Returns:
 */
int queue_empty(queue* q)
{
	return vec__empty(q);
}

/* queue_size()
 *
 *
 * Parameters:
 * Returns:
 */
int queue_size(queue* q)
{
	return vec__size(q);
}
