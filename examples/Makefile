.PHONY: all clean
all: example_wrap example example_balancing tests

#VERSION=-version=rop -version=costlyinvariants -version=paranoid
VERSION=-version=rop

DDOC=-D -Ddddoc ddoc/candydoc/candy.ddoc ddoc/candydoc/modules.ddoc

DMD=dmd2
#DFLAGS=-w -debug -unittest -g -gc $(VERSION)
#DFLAGS=-w -profile -debug -unittest -g -gc $(DDOC) $(VERSION)
#DFLAGS=-w -debug -unittest $(DDOC) $(VERSION)
#DFLAGS=-w -O -inline -release -unittest $(DDOC) $(VERSION)
#DFLAGS=-w -O -inline -release $(VERSION)
#DFLAGS=-O -inline -release -g -gc $(VERSION)
DFLAGS=-O -inline -release $(VERSION)


tests: tests.d cord_l.d ../cords.d ../RBt.d
	$(DMD) $(DFLAGS) -oftests tests.d ../cord_l.d ../cords.d utils/timer.d utils/log.d ../RBt.d

example: example.d ../cords.d ../RBt.d
	$(DMD) $(DFLAGS) -ofexample example.d ../cords.d utils/timer.d utils/log.d ../RBt.d

example_balancing: example_balancing.d ../cords.d ../RBt.d
	$(DMD) $(DFLAGS) -ofexample_balancing example_balancing.d ../cords.d utils/timer.d utils/log.d ../RBt.d

example_wrap: example_wrap.d ../cords.d ../RBt.d
	$(DMD) $(DFLAGS) -ofexample_wrap example_wrap.d ../cords.d utils/timer.d utils/log.d ../RBt.d

example_readme: example_readme.d ../cords.d ../RBt.d
	$(DMD) $(DFLAGS) -ofexample_readme example_readme.d ../cords.d utils/timer.d utils/log.d ../RBt.d


clean:
	rm *.o
