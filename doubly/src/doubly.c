#include "doubly/doubly.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

List* list_create() {
    List* list = malloc(sizeof(List));
    list->first = NULL;
    list->last = NULL;

    return list;
}

void list_free(List** list) {
    if (list == NULL) {
        return;
    }

    Node* current = (*list)->first;
    Node* next;
    while (current != NULL) {
        next = current->next;
        free(current->data);
        free(current);
        current = next;
    }

    free(*list);
    *list = NULL;
}

void list_insert_after(List* list, Node* node, char* data) {
    if (list == NULL) {
        return;
    }

    Node* new_node = malloc(sizeof(Node));

    size_t data_length = strlen(data) + 1;
    new_node->data = calloc(data_length, 1);
    if (new_node->data == NULL)
    {
        free(new_node);
        return;
    }
    memcpy(new_node->data, data, data_length);

    new_node->prev = node;
    if (node->next == NULL) {
        new_node->next = NULL;
        list->last = new_node;
    } else {
        new_node->next = node->next;
        node->next->prev = new_node;
    }
    node->next = new_node;
}

void list_insert_before(List* list, Node* node, char* data) {
    if (list == NULL) {
        return;
    }

    Node* new_node = malloc(sizeof(Node));

    size_t data_length = strlen(data) + 1;
    new_node->data = calloc(data_length, 1);
    if (new_node->data == NULL)
    {
        free(new_node);
        return;
    }
    memcpy(new_node->data, data, data_length);

    new_node->next = node;
    if (node->prev == NULL) {
        new_node->prev = NULL;
        list->first = new_node;
    } else {
        new_node->prev = node->prev;
        node->prev->next = new_node;
    }
    node->prev = new_node;
}

void list_prepend(List* list, char* data) {
    if (list == NULL) {
        return;
    }

    if (list->first == NULL) {
        Node* new_node = malloc(sizeof(Node));
        size_t data_length = strlen(data) + 1;
        new_node->data = calloc(data_length, 1);
        if (new_node->data == NULL)
        {
            free(new_node);
            return;
        }
        memcpy(new_node->data, data, data_length);

        new_node->prev = NULL;
        new_node->next = NULL;

        list->first = new_node;
        list->last = new_node;
    } else {
        list_insert_before(list, list->first, data);
    }
}

void list_append(List* list, char* data) {
    if (list == NULL) {
        return;
    }

    if (list->last == NULL) {
        list_prepend(list, data);
    } else {
        list_insert_after(list, list->last, data);
    }
}

Node* list_find(List* list, char* data) {
    if (list == NULL) {
        return NULL;
    }

    Node* current = list->first;
    while (current != NULL) {
        if (strcmp(current->data, data) == 0) {
            return current;
        }
        current = current->next;
    }

    return NULL;
}

void list_delete(List* list, Node* node) {
    if (list == NULL ||
        list->first == NULL || 
        node == NULL) {
        return;
    }

    if (node->prev == NULL) {
        list->first = node->next;
    } else {
        node->prev->next = node->next;
    }

    if (node->next == NULL) {
        list->last = node->prev;
    } else {
        node->next->prev = node->prev;
    }

    free(node);
}

unsigned int list_length(List* list) {
    unsigned int length = 0;

    Node* node = list->first;
    while (node != NULL) {
        length++;
        node = node->next;
    }

    return length;
}

void list_visit_fwd(List* list, void (*fun)(Node*)) {
    if (list == NULL) {
        return;
    }

    Node* node = list->first;
    while (node != NULL) {
        fun(node);
        node = node->next;
    }
}

void list_visit_bwd(List* list, void (*fun)(Node*)) {
    if (list == NULL) {
        return;
    }

    Node* node = list->last;
    while (node != NULL) {
        fun(node);
        node = node->prev;
    }
}

void list_print(List* list) {
    if (list == NULL) {
        return;
    }

    printf("* -> ");
    Node* last;
    Node* node = list->first;
    while (node != NULL) {
        printf("'%s'", node->data);
        last = node;
        node = node->next;
        if (node != NULL) {
            printf(" -> ");
        }
    }
    if (last == NULL) {
        printf("NULL");
    }
    printf(" -> *");
}
