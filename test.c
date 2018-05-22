#ifdef __GLASGOW_HASKELL__
// #include "Hello_stub.h"
extern void __stginit_NgxRequest(void);
#endif

#include <dlfcn.h>
#include <stdio.h>
#include <HsFFI.h>

#include <ngx_core.h>
#include <ngx_http.h>

#include "test.h"

void *NgxRequest;
void (*print_request)(struct ngx_http_request_s *);

int
initialize(int argc, char *argv[])
{
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
}

void
shutdown_module()
{
  // hs_exit();
}

// int main(int argc, char *argv[])
// {
//   initialize(argc, argv);
//  
//   ngx_http_request_t request = {
//     .uri = ngx_string("/hellothere"), 
//     .args = ngx_string("?wow=nice"), 
//     .method_name = ngx_string("GET"),
//     .http_protocol = ngx_string("HTTP/1.1"),
//   };
// 
//   print_request(&request);
// 
//   shutdown_module();
//   return 0;
// }
