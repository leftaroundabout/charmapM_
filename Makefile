all : bin/charmapM_

HSC = ghc

install : bin/charmapM_
	cp bin/charmapM_ /usr/local/bin/charmapM_

bin/charmapM_ : main.hs
	$(HSC) --make main.hs -o bin/charmapM_
