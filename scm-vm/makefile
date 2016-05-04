ctri: ctri.scm centring.util.import.so centring.expand.import.so \
      centring.analyze.import.so centring.cps.import.so
	csc ctri.scm

clean:
	rm *.so *.import.scm ctri

%.import.so: %.scm
	csc -sJ $<
	csc -s $*.import.scm
