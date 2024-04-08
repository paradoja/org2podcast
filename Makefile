##
# Blulaktuko private podcasts
#
# @file
# @version 0.1

.PHONY: build test watch copy_bin

build:
	stack build

test:
	stack test

watch:
	stack test --file-watch

copy_bin: build
	echo "Copying binary TO ${DST} -  $$DST"
	nix develop -c bash -c "cp \\$$(stack path --local-install-root)/bin/org2podcast ${DST}"
# end
