syntax: src/RSTT/Syntax/Test

clean:
	make --makefile=src/RSTT/Makefile clean

src/RSTT/Syntax/Test: src/RSTT/Syntax.cf
	pushd src \
		&& bnfc -d RSTT/Syntax.cf -p RSTT --makefile=RSTT/Makefile \
		&& make --makefile=RSTT/Makefile ; \
		popd

.PHONY: syntax clean
