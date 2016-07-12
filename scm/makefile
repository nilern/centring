ctri: centring.util.import.so \
      centring.expand.import.so \
      centring.value.import.so \
      centring.env.import.so \
      centring.primops.import.so \
      centring.ast.import.so \
      centring.dispatch.import.so \
      centring.analyze.import.so \
      centring.cek.import.so \
      ctri.scm
	csc ctri.scm

clean:
	rm *.so *.import.scm ctri

%.import.so: %.scm
	csc -sJ $<
	csc -s $*.import.scm
