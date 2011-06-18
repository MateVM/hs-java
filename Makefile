GHC=ghc --make -fwarn-unused-imports

all: dump-class

dump-class: dump-class.hs */*.hs
	$(GHC) $<

clean:
	find . -name *.hi -delete
	find . -name *.o -delete
