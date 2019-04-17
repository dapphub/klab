all:
	happy -gca ParAct.y
	alex -g LexAct.x
	ghc --make TestAct.hs -o TestAct

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocAct.* LexAct.* ParAct.* LayoutAct.* SkelAct.* PrintAct.* TestAct.* AbsAct.* TestAct ErrM.* SharedString.* ComposOp.* act.dtd XMLAct.* Makefile*
	
