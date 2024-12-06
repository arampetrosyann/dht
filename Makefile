SRCS=src/*.erl
OUTDIR=./_build
EFLAGS=-o $(OUTDIR)
SFLAGS=-pa $(OUTDIR)

all:
	mkdir -p $(OUTDIR)
	erlc $(EFLAGS) $(SRCS)

run:
	make all
	erl $(SFLAGS)

clean:
	rm -rf $(OUTDIR)/*.beam
