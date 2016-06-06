ctri: centring.ast.import.so ctri.scm 
	csc -R r7rs ctri.scm

clean:
	rm *.so *.import.scm ctri

%.import.so: %.scm
	csc -R r7rs -sJ $<
	csc -R r7rs -s $*.import.scm
