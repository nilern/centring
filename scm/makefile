all: ctrc ctri

ctrc: array.import.so centring.expand.import.so centring.coreast.import.so \
      centring.cps.import.so centring.emit.import.so compiler.scm
	csc -o ctrc compiler.scm

ctri: array.import.so centring.expand.import.so centring.coreast.import.so \
      centring.cps.import.so centring.vm.import.so ctri.scm
	csc -o ctri ctri.scm

clean:
	rm *.so *.import.scm ctrc ctri

%.import.so: %.scm
	csc -sJ $<
	csc -s $*.import.scm
