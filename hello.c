#ifdef __GLASGOW_HASKELL__
// #include "Hello_stub.h"
extern void __stginit_NgxRequest(void);
#endif

#include <dlfcn.h>
#include <stdio.h>
#include <HsFFI.h>

#include <stdlib.h>
#include <stddef.h>
#include <string.h>

typedef struct {
  size_t len;
  char *data;
} string;

struct ngx_list_part_s {
    void             *elts;
    unsigned int     nelts;
    struct ngx_list_part_s *next;
};

struct ngx_list_s {
    struct ngx_list_part_s  *last;
    struct ngx_list_part_s   part;
    size_t            size;
    unsigned int      nalloc;
    void       *pool;
};

struct ngx_table_elt_s {
    unsigned int   hash;
    string         key;
    string         value;
    unsigned char *lowcase_key;
};

struct ngx_http_headers_in_s {
    struct ngx_list_s headers;
};

struct ngx_http_request_s {

  uint32_t                          signature;         /* "HTTP" */

  void                 *connection;

  void                            **ctx;
  void                            **main_conf;
  void                            **srv_conf;
  void                            **loc_conf;

  void         *read_event_handler;
  void         *write_event_handler;
  void         *read_event_handlers;

#if (NGX_HTTP_CACHE)
  void                 *cache;
#endif

  void              *upstream;
  void                      *upstream_states;
                                     /* of ngx_http_upstream_state_t */

  void                       *pool;
  void                        *header_in;

  struct ngx_http_headers_in_s             headers_in;
};

string
make_string(const char *str)
{
  size_t len = strlen(str);
  return (string){
    .len = len,
    .data = strdup(str),
  };
}

void
make_table_elt(struct ngx_table_elt_s *elt, const char *k, const char *v)
{
  elt->hash = 42;
  elt->key = make_string(k);
  elt->value = make_string(v);
  elt->lowcase_key = NULL;
}

int
make_list(struct ngx_list_s *list, unsigned int n, size_t size)
{
    list->part.elts = malloc(n * size);
    if (list->part.elts == NULL) {
        return -1;
    }

    list->part.nelts = 0;
    list->part.next = NULL;
    list->last = &list->part;
    list->size = size;
    list->nalloc = n;
    list->pool = NULL;

    return 0;
}

void *
ngx_list_push(struct ngx_list_s *l)
{
    void             *elt;
    struct ngx_list_part_s  *last;

    last = l->last;

    if (last->nelts == l->nalloc) {

        /* the last part is full, allocate a new list part */

        last = malloc(sizeof(struct ngx_list_part_s));
        if (last == NULL) {
            return NULL;
        }

        last->elts = malloc(l->nalloc * l->size);
        if (last->elts == NULL) {
            return NULL;
        }

        last->nelts = 0;
        last->next = NULL;

        l->last->next = last;
        l->last = last;
    }

    elt = (char *) last->elts + l->size * last->nelts;
    last->nelts++;

    return elt;
}


int main(int argc, char *argv[])
{
  void *NgxRequest;
  void (*print_request)(struct ngx_http_request_s *);

  hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
  hs_add_root(__stginit_NgxRequest);
#endif
 
  if ((NgxRequest = dlopen("libNgxRequest.so", RTLD_LAZY)) == NULL) {
    printf("NOPE\n");
    return 1;
  }
  if ((print_request = dlsym(NgxRequest, "print_request")) == NULL) {
    printf("NOPE HASH\n");
    return 1;
  }

  struct ngx_list_s list;
  make_list(&list, 2, sizeof(struct ngx_table_elt_s));

  struct ngx_table_elt_s *elm = ngx_list_push(&list);
  make_table_elt(elm, "key1", "val1");
  elm = ngx_list_push(&list);
  make_table_elt(elm, "key2", "val2");

  struct ngx_http_headers_in_s headers = {
    .headers = list,
  };

  struct ngx_http_request_s request = {
      
    .headers_in = headers,
  };

  printf("offset: %ld\n", offsetof(struct ngx_http_request_s, headers_in));
  print_request(&request);

  hs_exit();
  return 0;
}
