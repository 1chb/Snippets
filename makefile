help:
	@echo "Targets: deploy install"
	@echo "Variables: $(foreach var,SERVER BINDIR EXECUTABLE,$(var)=$($(var)))"

SERVER = base
BINDIR = $(HOME)/.cabal/bin
EXECUTABLE = Snippets
SUFFIX := $(shell date +'%F+%2H.%2M.%2S')

deploy: TARGET = $(EXECUTABLE)_$(SUFFIX)
deploy: install
	scp $(BINDIR)/$(EXECUTABLE) $(SERVER):$(TARGET)

install:
	cabal install --overwrite-policy=always exe:$(EXECUTABLE)
