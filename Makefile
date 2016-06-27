c2pl: c2pl.hs
	ghc c2pl.hs

c2pl.hs: c2pl.chs
	c2hs c2pl.chs

clean:
	rm -f c2pl c2pl.chi c2pl.chs.h c2pl.hi c2pl.o c2pl.hs
