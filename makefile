help:
	@echo "Targets: deploy install dev"
	@echo "Variables: $(foreach var,SERVER BINDIR EXECUTABLE,$(var)=$($(var)))"

SERVER = base
BINDIR = $(HOME)/.cabal/bin
EXECUTABLE = Snippets
DESTDIR := Snippets/$(shell date +'%F+%2H.%2M.%2S')

deploy: install
	ssh $(SERVER) 'mkdir -p $(DESTDIR)'
	scp $(BINDIR)/$(EXECUTABLE) $(SERVER):$(DESTDIR)/executable
	scp service.mk $(SERVER):$(DESTDIR)
	ssh $(SERVER) 'ln -fns $(DESTDIR) server'
	@echo "On the $(SERVER) server execute: ./server/service.mk NAME=<name of service> deploy"

install:
	cabal install --overwrite-policy=always exe:$(EXECUTABLE)

dev: install
	env ADMIN_PASSWORD=aGVtbGlndA== $(BINDIR)/$(EXECUTABLE) --local
