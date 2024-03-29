REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3
BUILD_WITHOUT_QUIC ?= true
export BUILD_WITHOUT_QUIC

ifeq ($(wildcard rebar3),rebar3)
REBAR3 = $(CURDIR)/rebar3
endif

REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")

ifeq ($(REBAR3),)
REBAR3 = $(CURDIR)/rebar3
endif

.PHONY: deps test build

all: build test docs

build: $(REBAR3)
	@$(REBAR3) compile

$(REBAR3):
	wget $(REBAR3_URL) || curl -Lo rebar3 $(REBAR3_URL)
	@chmod a+x rebar3

shell:
	@$(REBAR3) shell
deps:
	@$(REBAR3) get-deps
lint:
	@$(REBAR3) lint
dyalizer:
	@$(REBAR3) dialyzer
clean:
	@$(REBAR3) clean
typer:
	@$(REBAR3) typer
distclean: clean
	@$(REBAR3) delete-deps

docs:
	@$(REBAR3) edoc


test: 
	@$(REBAR3) do ct, cover


release: test
	@$(REBAR3) release
