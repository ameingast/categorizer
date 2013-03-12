target: build

target=./dist/build/categorizer/categorizer

configure:
	@cabal configure --enable-tests

build: configure
	@cabal build
	@hlint -c src
	@strip $(target)

clean:
	@cabal clean

doc:
	@cabal haddock --executables
