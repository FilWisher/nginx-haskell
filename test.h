#ifndef __TEST_H__
#define __TEST_H__

void *NgxRequest;
void (*print_request)(struct ngx_http_request_s *);
int initialize(int argc, char **argv);
void shutdown_module();

#endif
