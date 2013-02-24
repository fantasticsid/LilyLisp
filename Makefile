SRC=$(wildcard *.hs)

Lilylisp: $(SRC)
	ghc --make Lilylisp.hs

clean:
	rm *.hi
	rm *.o
	rm Lilylisp
