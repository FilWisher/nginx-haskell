#ifdef __GLASGOW_HASKELL__
// #include "Hello_stub.h"
extern void __stginit_NgxRequest(void);
#endif

#include <dlfcn.h>
#include <stdio.h>
#include <string.h>
#include <HsFFI.h>

#include <ngx_core.h>
#include <ngx_http.h>
#include "ngx_http_haskell_module.h"

#define HELLO_HASKELL "Hello Haskell!"

static char *ngx_http_haskell(ngx_conf_t *cf, ngx_command_t *cmd, void *conf);
static ngx_int_t ngx_http_haskell_handler(ngx_http_request_t *r);

void *NgxRequest = NULL;
typedef ngx_int_t (*handler_fn)(ngx_http_request_t *);

handler_fn printit;

struct haskell_module {
  void *module;  
  handler_fn handler;
};

static handler_fn handler;

int
initialize(struct haskell_module *mod_, const char *module_name, const char *handler_name)
{
  printf("INITIALIVZINIG\n");
  hs_init(0, 0);
  printf("ADDING ROOT\n");
#ifdef __GLASGOW_HASKELL__
  printf("GH!\n");
  hs_add_root(__stginit_NgxRequest);
#endif
 
  void *mod;

  printf("ROOT ADDED\n");
  // if ((mod = dlopen(module_name, RTLD_LAZY)) == NULL) {
  if ((mod = dlopen(module_name, RTLD_NOW)) == NULL) {
    fprintf(stderr, "NOPE\n");
    return -1;
  }
  printf("DLOPEND\n");
  // if ((handler = dlsym(mod, handler_name)) == NULL) {
  if ((handler = dlsym(mod, handler_name)) == NULL) {
    fprintf(stderr, "NOPE HASH\n");
    return -2;
  }

  fprintf(stderr, "RETURNING\n");
  return 0;
}

void
shutdown_module()
{
  hs_exit();
}

/**
 * This module provided directive: hello world.
 *
 */
static ngx_command_t ngx_http_haskell_commands[] = {

    { ngx_string("haskell"), /* directive */
      NGX_HTTP_LOC_CONF|NGX_CONF_TAKE2, /* location context and takes
                                            1 arguments*/
      ngx_http_haskell, /* configuration setup function */
      0, /* No offset. Only one context is supported. */
      0, /* No offset when storing the module configuration on struct. */
      NULL},

    ngx_null_command /* command termination */
};

/* The hello world string. */
static u_char ngx_hello_haskell[] = HELLO_HASKELL;

typedef struct ngx_http_haskell_loc_conf_s {
  struct haskell_module module; 
} ngx_http_haskell_loc_conf_t;

static void *
ngx_http_haskell_create_loc_conf(ngx_conf_t *cf)
{
  
    ngx_http_haskell_loc_conf_t *conf;

    conf = ngx_pcalloc(cf->pool, sizeof(*conf));
    if (conf == NULL) {
        return NULL;
    }

    return conf;
}

/* The module context. */
static ngx_http_module_t ngx_http_haskell_module_ctx = {
    NULL, /* preconfiguration */
    NULL, /* postconfiguration */

    NULL, /* create main configuration */
    NULL, /* init main configuration */

    NULL, /* create server configuration */
    NULL, /* merge server configuration */

    ngx_http_haskell_create_loc_conf, /* create location configuration */
    NULL /* merge location configuration */
};

/* Module definition. */
ngx_module_t ngx_http_haskell_module = {
    NGX_MODULE_V1,
    &ngx_http_haskell_module_ctx, /* module context */
    ngx_http_haskell_commands, /* module directives */
    NGX_HTTP_MODULE, /* module type */
    NULL, /* init master */
    NULL, /* init module */
    NULL, /* init process */
    NULL, /* init thread */
    NULL, /* exit thread */
    NULL, /* exit process */
    NULL, /* exit master */
    NGX_MODULE_V1_PADDING
};

/**
 * Content handler.
 *
 * @param r
 *   Pointer to the request structure. See http_request.h.
 * @return
 *   The status of the response generation.
 */

// TODO: get custom location config and read handler from it. Call handler
// passing in `ngx_haskell_status_t` struct. Return 
static ngx_int_t ngx_http_haskell_handler(ngx_http_request_t *r)
{
    ngx_buf_t *b;
    ngx_chain_t out;
    ngx_http_haskell_loc_conf_t *hscf;
    int rc;
    // handler_fn handler;

    hscf = ngx_http_get_module_loc_conf(r, ngx_http_haskell_module);
    //handler = hscf->module.handler;

    fprintf(stderr, "addr: %p\n", r);

    /* Allocate a new buffer for sending out the reply. */
    b = ngx_pcalloc(r->pool, sizeof(ngx_buf_t));

    // NOTE: hscf->handler shouldn't be NULL here
    if (handler != NULL) {
      rc = handler(r);
      printf("RC: %d\n", rc);
    } else
      printf("NOPE\n");

    return rc;
    
} /* ngx_http_haskell_handler */

/**
 * Configuration setup function that installs the content handler.
 *
 * @param cf
 *   Module configuration structure pointer.
 * @param cmd
 *   Module directives structure pointer.
 * @param conf
 *   Module configuration structure pointer.
 * @return string
 *   Status of the configuration setup.
 */
static char *ngx_http_haskell(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_haskell_loc_conf_t *hscf = conf;
    ngx_http_core_loc_conf_t *clcf; /* pointer to core location configuration */
    ngx_str_t *args;
    char *module_name, *handler_name;


    args = cf->args->elts;
    module_name = strndup(args[1].data, args[1].len);
    printf("module_name: %s\n", module_name);
    handler_name = strndup(args[2].data, args[2].len);
    printf("handler_name: %s\n", handler_name);
    
  int err = initialize(&hscf->module, module_name, handler_name);
  if (err == -1)
    return "Unable to load dynamic Haskell module";
  else if (err == -2)
    return "Unable to load handler from Haskell module";
    

    /* Install the hello world handler. */
    clcf = ngx_http_conf_get_module_loc_conf(cf, ngx_http_core_module);
    clcf->handler = ngx_http_haskell_handler;

    return NGX_CONF_OK;
}

int
ngx_http_haskell_send_headers(ngx_http_request_t *r, int status)
{
    fprintf(stderr, "HERE I AM SENDING HEADERS\n");

    /* Set the Content-Type header. */
    r->headers_out.content_type.len = sizeof("text/plain") - 1;
    r->headers_out.content_type.data = (u_char *) "text/plain";

    /* Sending the headers for the reply. */
    r->headers_out.status = status;
    /* Get the content length of the body. */
    // TODO: work this out correctly
    // r->headers_out.content_length_n = 23;

    return ngx_http_send_header(r); /* Send the headers */
}

int
// ngx_http_haskell_write_chunk(ngx_http_request_t *r, ngx_str_t chunk)
ngx_http_haskell_write_chunk(ngx_http_request_t *r, char *chunk)
{
    ngx_buf_t    *buf;
    ngx_chain_t   out;

    printf("WRITE CHCUNK\n");
    // printf("chunk len: %ld\n", chunk.len);
    // char *str = strndup(chunk.data, chunk.len);
    printf("chunk: %s\n", chunk);
    size_t size = strlen(chunk);
    printf("len: %ld\n", size);
   
    printf("addr: %p\n", r);

    buf = ngx_pcalloc(r->pool, sizeof(ngx_buf_t));
    if (buf == NULL) {
        fprintf(stderr, "WRITING\n");
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0, 
            "Failed to allocate response buffer.");
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    // buf->start = malloc(size);
    // printf("WRITE CHCUNK HERO\n");
    // buf->end = buf->start + size;
    // buf->pos = buf->end;
    // buf->temporary = 1;
    // 

    // memmove(buf->start, chunk, size);


    // fprintf(stderr, "WRITING\n");
    buf->start = chunk;
    buf->pos = chunk; /* first position in memory of the data */
    buf->last = chunk + strlen(chunk); /* last position */
    buf->end = chunk + strlen(chunk);
 
    // Content should be copied. We don't want Haskell's GC to collect the
    // buffer before it gets sent.
    buf->memory = 1;
    // there will be more
    buf->last_buf = 0;

    out.buf = buf;
    out.next = NULL;

    return ngx_http_output_filter(r, &out);
}

static char NL = '\n';

int
ngx_http_haskell_flush(ngx_http_request_t *r)
{
    ngx_buf_t    *buf;
    ngx_chain_t   out;

    fprintf(stderr, "FLUSHING\n");
    buf = ngx_pcalloc(r->pool, sizeof(ngx_buf_t));
    if (buf == NULL) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0, 
            "Failed to allocate response buffer.");
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    // Empty final buffer
    buf->pos = NULL;
    buf->last = NULL;
    buf->memory = 0;
    buf->last_buf = 1;

    out.buf = buf;
    out.next = NULL;

    return ngx_http_output_filter(r, &out);
}
