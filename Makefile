GHC=ghc --make -fwarn-unused-imports

all: disassemble

disassemble: disassemble.hs */*.hs
	$(GHC) $<

clean:
	find . -name *.hi -delete
	find . -name *.o -delete
