hcat.cabal:
	hpack .

.PHONY: hcat
hcat: hcat.cabal
	cabal build exec:hcat

.PHONY: test
test: hcat.cabal
	cabal test --test-show-detail=direct
