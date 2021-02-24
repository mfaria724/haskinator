all: Oraculo.hs Haskinator.hs
	ghc -o Haskinator Haskinator.hs
	$(MAKE) clean

haskinator: Oraculo.hs Haskinator.hs
	ghc -o Haskinator Haskinator.hs

clean: 
	$(RM) Oraculo.o Oraculo.hi Haskinator.o Haskinator.hi     
