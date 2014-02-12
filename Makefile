####################
GROUP_NUMBER := 09
####################

ERL := erl
ERLC := erlc
ERLC_FLAGS := -W -I include
ERLINTER = /opt/local/lib/erlang/lib/erl_interface-3.7.2

ERL_FILES := $(wildcard src/*.erl)
BEAM_FILES := $(patsubst src/%.erl,ebin/%.beam,${ERL_FILES})

comma:= ,
empty:=
space:= $(empty) $(empty)

EDOC_SRC := $(filter-out %_test.erl, $(ERL_FILES))
EDOC_SRC_LIST := [$(subst $(space),$(comma),$(patsubst src/%.erl,'src/%.erl', $(EDOC_SRC)))]

REQUIRED_DIR_NAME := pop_2011_group_$(GROUP_NUMBER)
PROJECT_DIR := $(notdir $(shell pwd))
ARCHIVE_NAME :=  $(REQUIRED_DIR_NAME)_archive__$(shell date "+%Y-%m-%d__%H:%M:%S")__.tar.gz
ARCHIVE_DIR := ..

NR := 1
NAME := yaba

all: $(BEAM_FILES)

ebin/%.beam: src/%.erl
	$(ERLC) $(ERLC_FLAGS) -o ebin $<

start: all
	cp src/yaba.app ebin/
	cd ebin && $(ERL) -name $(NAME) -eval "application:start(yaba)" -s

test: all
	(cd ebin && erl -noinput -eval 'eunit:test({dir, "."}, [verbose]), init:stop()')

doc: $(BEAM_FILES)
	erl -noshell -eval "edoc:files($(EDOC_SRC_LIST), [{dir, 'doc/html'}])" -s init stop

cnode:
	gcc -o ./ebin/cnode -I$(ERLINTER)/include -L$(ERLINTER)/lib src/cnode.c -lerl_interface -lei

arrow:
	gcc -o ./ebin/arrow -I$(ERLINTER)/include -L$(ERLINTER)/lib src/arrow.c -lerl_interface -lei

start-arrow: arrow
	cd ebin && ./arrow yaba@$(shell hostname) $(NR)

taxi: cnode
	cd ebin && ./cnode yaba@$(shell hostname) taxi $(NR) ../data/taxi$(NR).txt

clean:
	rm -fr .#* *.dump
	rm -fr ebin/*.beam
	(cd doc/html && ls | grep -v ^overview.edoc$ | xargs rm)

remove_finderinfo:
	-xattr -d "com.apple.FinderInfo" src/*.erl include/*.hrl doc/* doc/html/*

archive: clean
ifeq ($(REQUIRED_DIR_NAME), $(PROJECT_DIR))
	(cd $(ARCHIVE_DIR) && tar cvfz $(ARCHIVE_NAME) $(PROJECT_DIR) )
	@echo 
	@echo NOTE: Archive created in $(ARCHIVE_DIR)/$(ARCHIVE_NAME)
	@echo 
else
	@echo Error: Wrong directory name $(PROJECT_DIR), change to $(REQUIRED_DIR_NAME)
endif
