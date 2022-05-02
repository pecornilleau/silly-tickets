.PHONY:build test
build:
	./ligo compile contract contracts/silly.mligo > silly.tz

test:
	./ligo run test tests/silly.mligo

