ctri: ctri.scm array.import.so centring.util.import.so \
      centring.vm.import.so centring.instructions.import.so \
      centring.expand.import.so centring.analyze.import.so \
      centring.cps.import.so
	csc ctri.scm

clean:
	rm *.so *.import.scm ctri

%.import.so: %.scm
	csc -sJ $<
	csc -s $*.import.scm
