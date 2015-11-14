PROGRAMS=hello

.SUFFIXES: .hs
.hs.o:
	ghc -c $@ $<

Spellbinder/SpellBook.hs: spells.txt SpellBookParser
	./SpellBookParser < spells.txt > $@ || { rm -f $@; exit 1; }

SpellBookParser: SpellBookParser.hs SpellBinder/Gestures.hs
	ghc SpellBookParser.hs

.PHONY: clean
clean:
	rm -f *.o *.hi Spellbinder/*.o Spellbinder/*.hi Spellbinder/SpellBook.hs SpellBookParser $(PROGRAMS)
