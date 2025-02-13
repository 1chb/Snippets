#!/usr/bin/make -f

### BEGIN INIT INFO
# Provides:		baseServer
# Required-Start:	$network $remote_fs $syslog postgresql
# Required-Stop:	$network $remote_fs $syslog postgresql
# Default-Start:	2 3 4 5
# Default-Stop:		0 1 6
# Short-Desciption:	Base functionallity, including greetings.
### END INIT INFO

# Assign before include directives to avoid them to be in the list:
MKFILE := $(lastword $(MAKEFILE_LIST))
SECRETFILE := /etc/$(NAME)/secrets

-include $(SECRETFILE)
-include /etc/$(NAME)/environment

NAME = baseServer
SRCDIR = $(dir $(MKFILE)).
EXECUTABLE = /opt/$(NAME)/executable
LOGFILE = /var/log/$(NAME).log
PIDFILE = /var/run/$(NAME).pid

.PHONY: help
help:
	@echo "$(MKFILE) NAME=<service name> <command>"; \
	echo "  where <command> is one of:"; \
	echo "    deploy                         -- performs install and restart"; \
	echo "    install                        -- installs this version even if the service is running"; \
	echo "    update-rc                      -- configures the service"; \
	echo "    PWD=<admin password> password  -- sets password which must be base64 encoded"; \
	echo "    start/stop/restart/status      -- requires sudo (used by init V service system)"

.PHONY: deploy
deploy: install
	sudo service $(NAME) restart

.PHONY: restart
restart: stop start

echoMsg = echo "$(NAME) is $1 with pid=(`cat $(PIDFILE)`)."
echoMsgNot = echo "$(NAME) is not $1."
isRunning = [ -f $(PIDFILE) ] && kill -0 `cat $(PIDFILE)` 2> /dev/null

.PHONY: status
status:
	@if $(isRunning); then \
	  $(call echoMsg,running); \
	else \
	  $(call echoMsgNot,available); \
	fi

.PHONY: start
start:
	@if $(isRunning); then \
	  $(call echoMsg,already running); \
	else \
	  date "+Start $(NAME) %Y-%m-%d %H:%M:%S.%N" >> $(LOGFILE); \
	  nohup env $(ENVIRONMENT) $(SECRETS) $(EXECUTABLE) >> $(LOGFILE) 2>&1 & echo $$! > $(PIDFILE); \
	  $(call echoMsg,started); \
	fi

.PHONY: stop
stop:
	@if $(isRunning); then \
	  date "+Stop $(NAME) %Y-%m-%d %H:%M:%S.%N" >> $(LOGFILE); \
	  kill -15 `cat $(PIDFILE)` && $(call echoMsg,stopped) && rm -f $(PIDFILE); \
	else \
	  $(call echoMsgNot,running); \
	fi

.PHONY: install
install: SERVICE = /etc/init.d/$(NAME)
install: TMP = /tmp/$(NAME)
install:
	sed "s/\$$(NAME)/$(NAME)/g" < $(SRCDIR)/service.mk > $(TMP)
	sudo mv $(TMP) $(SERVICE)
	sudo chown root:root $(SERVICE)
	sudo chmod 755 $(SERVICE)
	sudo mkdir -p $(dir $(EXECUTABLE))
	sudo cp $(SRCDIR)/executable $(TMP)
	sudo mv $(TMP) $(EXECUTABLE)

.PHONY: update-rc
update-rc:
	sudo update-rc.d -f $(NAME) remove
	sudo update-rc.d $(NAME) defaults

.PHONY: password
password: TMP = /tmp/$(NAME)
password:
	sudo mkdir -p $(dir $(SECRETFILE))
	echo "ADMIN_PASSWORD = $(PWD)" > $(TMP)
	echo 'SECRETS = ADMIN_PASSWORD=$$(ADMIN_PASSWORD)' >> $(TMP)
	sudo cp $(TMP) $(SECRETFILE)
	sudo chmod go-rwx $(SECRETFILE)
	rm -f $(TMP)
