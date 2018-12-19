MODULES = main_strategy_matrix.m givetake.m givetake.action.m	\
givetake.game.m givetake.parameters.m givetake.player.m givetake.strategy.m

MFLAGS = --output-compile-error-lines 100 --make --verbose-error-messages	\
--max-error-line-width 120 --grade asm_fast.gc \
--mercury-library-directory /home/pedro/local/lib/mercury \
--mercury-library ui_console

BIN = main_strategy_matrix

all: $(BIN)

$(BIN): $(MODULES)

%: %.m
	mmc -m $(MFLAGS) $@

clean:
	rm -f *~ *.err *.mh

cleanall: clean
	rm -fr ./Mercury/
	rm -f .
