#include "doubly/doubly.h"

#include <stdio.h>
#include <assert.h>

int main()
{
    printf("Hello, C!\n");

    printf("Create a doubly linked list with elements from 0 to 5.\n");
    List* list = list_create();
    list_append(list, "3");
    list_append(list, "4");
    list_append(list, "5");
    list_prepend(list, "2");
    list_prepend(list, "1");
    list_prepend(list, "0");
    assert(list_length(list) == 6 && "List should contain 6 elements.");
    list_print(list);

    printf("Delete first and last element.\n");
    list_delete(list, list->first);
    list_delete(list, list->last);
    assert(list_length(list) == 4 && "List should contain 4 elements.");
    list_print(list);

    Node* node;
    node = list_find(list, "3");
    assert(node && "A node labeled '3' should be found.");
    node = list_find(list, 0);
    assert(node == NULL && "Node 0 should not be found.");
    node = list_find(list, 5);
    assert(node == NULL && "Node 5 should not be found.");

    list_free(&list);
    assert(node == NULL && "List should have been freed.");

    return 0;
}
