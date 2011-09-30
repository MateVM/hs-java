GHC=ghc --make -fwarn-unused-imports

all: dump-class rebuild-class TestGen

dump-class: dump-class.hs */*.hs
	$(GHC) $<

rebuild-class: rebuild-class.hs */*.hs
	$(GHC) $<

TestGen: TestGen.hs */*.hs
	$(GHC) $<

clean:
	find . -name *.hi -delete
	find . -name *.o -delete
