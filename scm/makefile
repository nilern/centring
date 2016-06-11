ctri: centring.util.import.so centring.expand.import.so centring.analyze.import.so \
      centring.rt.import.so centring.ns.import.so centring.eval-ast.import.so \
      ctri.scm 
	csc ctri.scm

clean:
	rm *.so *.import.scm ctri

%.import.so: %.scm
	csc -sJ $<
	csc -s $*.import.scm
