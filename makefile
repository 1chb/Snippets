help:
	@echo "Targets: deploy install dev test"
	@echo "Variables: $(foreach var,SERVER BINDIR EXECUTABLE,$(var)=$($(var)))"

SERVER = base
BINDIR = $(HOME)/.cabal/bin
EXECUTABLE = Snippets
DESTDIR := Snippets/$(shell date +'%F+%2H.%2M.%2S')

.PHONY: deploy
deploy: install
	ssh $(SERVER) 'mkdir -p $(DESTDIR)'
	scp $(BINDIR)/$(EXECUTABLE) $(SERVER):$(DESTDIR)/executable
	scp service.mk $(SERVER):$(DESTDIR)
	ssh $(SERVER) 'ln -fns $(DESTDIR) server'
	@echo "On the $(SERVER) server execute: ./server/service.mk NAME=<name of service> deploy"

.PHONY: install
install:
	cabal install --overwrite-policy=always exe:$(EXECUTABLE)
	strip $(BINDIR)/$(EXECUTABLE)

.PHONY: dev
dev: install
	env ADMIN_PASSWORD=aGVtbGlndA== $(BINDIR)/$(EXECUTABLE) --development

.PHONY: test
test:
	cabal test $(call whenDef,--test-options='--match=,$(MATCH),')

whenDef = $(if $(2:''=),$1$2$3,)
