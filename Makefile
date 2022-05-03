.PHONY:build test
build:
	./ligo compile contract contracts/silly.mligo > silly.tz

test:
	./ligo run test with_storage/silly.mligo
	./ligo run test tests/silly.mligo
	./ligo run test without_storage/silly.mligo
	./ligo run test without_storage/wallet.mligo

