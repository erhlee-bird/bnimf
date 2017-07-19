.PHONY: all clean

all:
	nim c bnimf.nim

clean:
	rm -rf nimcache bnimf
