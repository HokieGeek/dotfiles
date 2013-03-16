////////////////////////////////////////////////////
//
// Filename: $RCSfile: vec.c,v $
//
// Description:
//
// $Log: vec.c,v $
// Revision 1.1  2006/08/31 15:26:21  per47214c
// Added the new files
//
////////////////////////////////////////////////////

////// Headers //////
#include <stdlib.h>
#include "vec.h"

////// Macros //////
#define FALSE 0
#define TRUE !(FALSE)

////// Main Code //////
/// MUTATORS ///
/* vec__list_init()
 *
 *
 * Parameters:
 * Returns:
 */
struct vec* vec__list_init()
{
	// Local Variables //
	struct vec* list = NULL;

	// Allocate the space
	list = (struct vec*)malloc(sizeof(struct vec));
	if (list == NULL) return NULL;

	list->head = list->tail = list->curr = NULL;
	list->size = 0;
	list->pos = -1;

	return list;
}

/* vec__list_destroy()
 *
 *
 * Parameters:
 * Returns:
 */
int vec__list_destroy(struct vec* list)
{
	//if ((list == NULL) || vec__empty(list)) return (FALSE);
	if (list == NULL) return (FALSE);

	// Traverse from the tail and destory each node
	if (!vec__empty(list)) {
		vec__walk_tail(list);
		while (list->curr != NULL) {
			if (list->curr != list->head) {
				vec__walk_r(list);
				if (list->curr != NULL) vec_node__destroy(list->curr->next);
			} else {
				if (list->curr != NULL) vec_node__destroy(list->curr);
				list->curr = NULL;
			}
		}
	}

	// Deallocate the list	
	free(list);

	return (TRUE);
}

/* vec__insert()
 *
 *
 * Parameters:
 * Returns:
 */
int vec__insert(struct vec* list, char* data)
{
	// Local Variables //
	struct vec_node* new = NULL;

	if ((list == NULL) || (data == NULL)) return (FALSE);
	
	// 
	new = vec_node__init(data, NULL, NULL);
	if (new == NULL) return (FALSE);

	// 
	if (vec__empty(list)) {
		list->head = list->tail = new;
	} else {
		if (list->curr == NULL) return (FALSE);
	
		new->prev = list->curr;

		if (list->curr == list->tail)
			list->tail = new;
		else {
			new->next = list->curr->next;
			list->curr->next->prev = new;
		}
	
		list->curr->next = new;
	}
	list->curr = new;
	list->pos++;

	list->size++;
	
	return (TRUE);
}

/* vec__delete()
 *
 *
 * Parameters:
 * Returns:
 */
int vec__delete(struct vec* list)
{
	if ((list == NULL) || (list->curr == NULL)) return (FALSE);
	
	if (list->head == list->tail) {
		vec_node__destroy(list->curr);
		list->head = list->tail = list->curr = NULL;
		list->pos = -1;
	} else if (list->curr == list->head) {
		list->head = list->curr->next;
		list->head->prev = NULL;
		vec_node__destroy(list->curr);
		list->curr = list->head;
		list->pos = 0;
	} else if (list->curr == list->tail) {
		list->tail = list->curr->prev;
		list->tail->next = NULL;
		vec_node__destroy(list->curr);
		list->curr = list->tail;
		list->pos = list->size-1;
	} else {
		list->curr->next->prev = list->curr->prev;
		list->curr->prev = list->curr->next;
		vec_node__destroy(list->curr);
		// TODO
		//list->curr = ?????
		list->pos--;
	}

	list->size--;

	return (TRUE);
}

/// REPORTERS ///

/* vec__walk_head()
 *
 *
 * Parameters:
 * Returns:
 */
int vec__walk_head(struct vec* list)
{
	if (list->head == NULL) return (FALSE);
	list->curr = list->head;
	list->pos = 0;
	return (TRUE);
}

/* vec__walk_tail()
 *
 *
 * Parameters:
 * Returns:
 */
int vec__walk_tail(struct vec* list)
{
	if ((list == NULL) || (list->tail == NULL)) return (FALSE);
	list->curr = list->tail;
	list->pos = list->size-1;
	return (TRUE);
}

/* vec__walk()
 *
 *
 * Parameters:
 * Returns:
 */
int vec__walk(struct vec* list)
{
	if ((list == NULL) || (list->curr == NULL)) return (FALSE);
	list->curr = list->curr->next;
	list->pos++;
	return (TRUE);
}

/* vec__walk_r()
 *
 *
 * Parameters:
 * Returns:
 */
int vec__walk_r(struct vec* list)
{
	if ((list == NULL) || (list->curr == NULL)) return (FALSE);
	list->curr = list->curr->prev;
	list->pos--;
	return (TRUE);
}

/* vec__get_current()
 *
 *
 * Parameters:
 * Returns:
 */
char* vec__get_current(struct vec* list)
{
	if (list->curr == NULL) return NULL;
	return list->curr->data;
}

/* vec__at()
 *
 *
 * Parameters:
 * Returns:
 */
char* vec__at(struct vec* list, int pos)
{
	// Local Variables //
	//int i = -1;

	if ((list == NULL) || (pos < 0)) return NULL;

	if (pos < list->pos)
		while(list->pos != pos)
			vec__walk_r(list);
	else if (pos > list->pos)
		while(list->pos != pos)
			vec__walk(list);

	/*if (pos < list->pos)
		for(i = list->pos; i > pos; i--)
			vec__walk_r(list);
	else if (pos > list->pos)
		for(i = list->pos; i < pos; i++)
			vec__walk(list);*/

	return list->curr->data;
}

/* vec__empty()
 *
 *
 * Parameters:
 * Returns:
 */
int vec__empty(struct vec* list)
{
	if ((list == NULL) || (list->size <= 0) || (list->head == NULL) || (list->tail == NULL)) return (TRUE);
	return (FALSE);
}

/* vec__size()
 *
 *
 * Parameters:
 * Returns:
 */
int vec__size(struct vec* list)
{
	return (list == NULL) ? 0 : list->size;
}
