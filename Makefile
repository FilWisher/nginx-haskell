NGINX_SRC=/home/william/src/nginx

ALL_INCS = \
	-I$(NGINX_SRC)/src/core \
	-I$(NGINX_SRC)/src/event \
	-I$(NGINX_SRC)/src/event/modules \
	-I$(NGINX_SRC)/src/os/unix \
	-I$(NGINX_SRC)/objs \
	-I$(NGINX_SRC)/src/http \
	-I$(NGINX_SRC)/src/http/modules \
	-I/usr/include

HASKELL_SRC := $(shell find haskell/src -name *.hs*)
haskell/libNginxHS.so: $(HASKELL_SRC)
	cd haskell && cabal new-build

test.o: haskell/libNgxRequest.so test.c
	ghc -static -c -no-hs-main -optc-O $(ALL_INCS) test.c  -L./haskell -o test.o -lNgxRequest -optl-Wl,-rpath,$(PWD)/haskell

ngx_http_haskell_module.o: haskell/libNginxHS.so src/ngx_http_haskell_module.c src/ngx_http_haskell_module.h
	ghc -static -c -no-hs-main -optc-O $(ALL_INCS) src/ngx_http_haskell_module.c  -L./haskell \
	  -o ngx_http_haskell_module.o -lNgxRequest -optl-Wl,-rpath,$(PWD)/haskell
