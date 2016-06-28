ctri: centring.util.import.so centring.value.import.so centring.primops.import.so \
      centring.expand.import.so centring.ast.import.so \
      centring.analyze.import.so centring.dispatch.import.so \
      centring.cps.import.so \
      centring.rt.import.so centring.ns.import.so \
      centring.interpret.import.so \
      ctri.scm 
	csc ctri.scm

clean:
	rm *.so *.import.scm ctri

%.import.so: %.scm
	csc -sJ $<
	csc -s $*.import.scm
