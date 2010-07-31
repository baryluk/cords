.PHONY: all clean doc
all: doc

#VERSION=-version=rop -version=costlyinvariants -version=paranoid
VERSION=-version=rop

DDOC=-D -Ddddoc ddoc/candydoc/candy.ddoc ddoc/candydoc/modules.ddoc

DMD=dmd2
#DFLAGS=-w -debug -unittest -g -gc $(VERSION)
#DFLAGS=-w -profile -debug -unittest -g -gc $(DDOC) $(VERSION)
DFLAGS=-w -debug -unittest $(DDOC) $(VERSION)
#DFLAGS=-w -O -inline -release -unittest $(DDOC) $(VERSION)
#DFLAGS=-w -O -inline -release $(VERSION)
#DFLAGS=-O -inline -release -g -gc $(VERSION)
#DFLAGS=-O -inline -release $(VERSION)


doc:
	$(DMD) $(DFLAGS) -o- *.d

clean:
	rm *.o
