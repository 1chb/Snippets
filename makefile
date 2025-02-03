help:
	@echo "Targets: deploy"
	@echo "Variables:$(foreach var,SERVER, $(var)=$($(var)))"

SERVER = base

deploy:
	cabal install --overwrite-policy=always
	scp /home/christian/.cabal/bin/Snippets $(SERVER):Snippets_`date +'%F+%2H.%2M.%2S'`
