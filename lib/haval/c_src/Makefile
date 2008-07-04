.PHONY: clean
.SUFFIXES: .o .c .cc

OS= ${shell uname}
CC=gcc
CXX=g++
CXXFLAGS=-Wall -g

# Erlang
ERL_INCLUDE = -I/usr/local/lib/erlang/usr/include 
ERL_LIBS    = -L/usr/local/lib/erlang/usr/lib \
              -lerts
EI_INCLUDE  = -I/usr/local/lib/erlang/lib/erl_interface-3.5.6/include
EI_LIBS     = -L/usr/local/lib/erlang/lib/erl_interface-3.5.6/lib \
              -lei \
              -lerl_interface

TARGET_LIB = haval_drv.so
ifeq ($(OS), Darwin)
  EXTRA_OPTIONS = -fno-common -bundle -undefined suppress -flat_namespace
endif
ALL: $(TARGET_LIB)

.c.o:
	$(CC) $(CFLAGS) -c $<

.cc.o:
	$(CXX) $(CXXFLAGS) $(ERL_INCLUDE) $(EI_INCLUDE) -c $<

haval_drv.so: haval.o haval_drv.o
	$(CXX) -o $@ $^ $(ERL_LIBS) $(EI_LIBS) $(EXTRA_OPTIONS) -fpic -O2
	mv haval_drv.so ../priv/lib 

clean:
	rm -f *.o *.so
