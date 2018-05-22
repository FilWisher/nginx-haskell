#ifndef __NGX_HTTP_HASKELL_WODULE__
#define __NGX_HTTP_HASKELL_WODULE__

#define MAX_STATUS_MESSAGE (80)

typedef struct ngx_haskell_status_s {
  unsigned int status_code;
  char msg[MAX_STATUS_MESSAGE];
} ngx_haskell_status_t;

// Opaque structure we don't need visibility into
typedef struct ngx_haskell_response_s {
  ngx_haskell_status_t status;
} ngx_haskell_response_t;

int ngx_http_haskell_flush(ngx_http_request_t *r);
// int ngx_http_haskell_write_chunk(ngx_http_request_t *r, ngx_str_t chunk);
int ngx_http_haskell_write_chunk(ngx_http_request_t *r, char *);
int ngx_http_haskell_send_headers(ngx_http_request_t *r, int status);

#endif
