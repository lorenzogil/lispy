CFLAGS=-std=c99 -Wall
DEPS=mpc.h
OBJ=clispy.o mpc.o
LIBS=-ledit

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

clispy: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)

.PHONY: clean

clean:
	rm -f $(OBJ) clispy
