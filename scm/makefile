SOS=array.so centring.expand.so centring.coreast.so centring.cps.so centring.emit.so centring.vm.so
ISOS=$(SOS:.so=.import.so)

ctrc: $(SOS) $(ISOS) compiler.scm
	csc -o ctrc compiler.scm

ctri: $(SOS) $(ISOS) ctri.scm
	csc -o ctri ctri.scm

clean:
	rm *.so *.import.scm ctrc

%.so %.import.scm: %.scm
	csc -sJ $<

%.import.so: %.import.scm
	csc -s $<
