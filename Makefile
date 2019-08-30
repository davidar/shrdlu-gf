RGL = ../gf-rgl/dist/
GF = gf --gf-lib-path=$(RGL)

build: BlocksEng.gf $(RGL)
	$(GF) --make --output-format=haskell $<

test: build
	$(GF) --run < test.gfs > test.log
	stack build && stack exec blockparse-exe < test.in > test.out

$(RGL):
	cd $(RGL)/.. && stack runghc Setup.hs build
