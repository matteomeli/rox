#ifndef _DOUBLY_H_
#define _DOUBLY_H_

typedef struct Node
{
    char* data;
    struct Node* prev;
    struct Node* next;
} Node;

typedef struct List
{
    struct Node* first;
    struct Node* last;
} List;

List* list_create();

void list_free(List** list);

void list_insert_after(List* list, Node* node, char* data);

void list_insert_before(List* list, Node* node, char* data);

void list_prepend(List* list, char* data);

void list_append(List* list, char* data);

Node* list_find(List* list, char* data);

void list_delete(List* list, Node* node);

unsigned int list_length(List* list);

void list_visit_fwd(List* list, void (*fun)(Node*));

void list_visit_bwd(List* list, void (*fun)(Node*));

void list_print(List* list);

#endif // _DOUBLY_H_
