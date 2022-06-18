syntax: src/RSTT/Syntax/Test

clean:
	cd src && (make --makefile=RSTT/Makefile clean; cd ../)

src/RSTT/Syntax/Test: src/RSTT/Syntax.cf
	cd src/ \
		&& bnfc -d RSTT/Syntax.cf -p RSTT --makefile=RSTT/Makefile \
		&& make --makefile=RSTT/Makefile ; \
		cd ../

.PHONY: syntax clean
