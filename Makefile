RGL = ../gf-rgl/dist/
GF = gf --gf-lib-path=$(RGL)

build: BlocksEng.gf $(RGL)
	$(GF) --make --output-format=haskell $<

test: $(RGL)
	$(GF) --run < test.gfs > test.log

$(RGL):
	cd $(RGL)/.. && stack runghc Setup.hs build
