GHC=ghc --make -fwarn-unused-imports

all: Hello.class dump-class rebuild-class TestGen

Hello.class: Hello.java
	javac $<

dump-class: dump-class.hs */*.hs
	$(GHC) $<

rebuild-class: rebuild-class.hs */*.hs
	$(GHC) $<

TestGen: TestGen.hs */*.hs
	$(GHC) $<

clean:
	find . -name *.hi -delete
	find . -name *.o -delete
