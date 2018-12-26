/**
 * This program generates a 1024x1024 matrix with the payoffs of the
 * history Give-and-Take strategies.

 * @author Pedro Mariano
 * @version 1.0 2018/12/19
 */
:- module main_strategy_matrix.

:- interface.

:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is cc_multi.

:- implementation.

:- import_module givetake, givetake.action, givetake.game, givetake.parameters, givetake.strategy, givetake.strategy.encodecode.
:- import_module bool, char, exception, getopt, list, maybe, solutions, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type commandLineOptions --->
	%% main game parameters
	bg       ;
	cpt      ;
	cst      ;
	%% other game parameters
	numberStages ;
	%%
	filename ;
	help
	.

:- type row --->
	row(
		strategy1 :: givetake.strategy.strategy,
		strategy2 :: givetake.strategy.strategy,
		payoff1   :: float,
		payoff2   :: float,
		godel1    :: int,
		godel2    :: int
	).

:- inst row == bound(row(hist, hist, ground, ground, ground, ground)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

main(!IO) :-
	io.command_line_arguments(Arguments, !IO),
	getopt.process_options(option_ops_multi(optionShort, optionLong, optionDefault), Arguments, RestArgs, IResult),
	(if
		RestArgs = []
	then
		(	% switch
			IResult = ok(Result),
			(if
				getopt.lookup_bool_option(Result, help) = yes
			then
				io.print(helpMessage, !IO)
			else
				GameParameters = gameParameters(Result),
				(if
					valid(GameParameters)
				then
					Filename = getopt.lookup_string_option(Result, filename),
					createPayoffMatrix(Filename, GameParameters, !IO),
					io.print("Done!\n", !IO)
				else
					io.print(io.stderr_stream, "Game parameters are invalid!\n", !IO),
					io.set_exit_status(1, !IO)
				)
			)
		;	
			IResult = error(Message),
			io.format(io.stderr_stream, "Error processing options:\n%s\n", [s(Message)], !IO),
			io.set_exit_status(1, !IO)
		)
	else
		io.format(io.stderr_stream, "Unrecognised options: %s\n", [s(string(RestArgs))], !IO)
	)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func helpMessage = string.

helpMessage = "Create a CSV file with the payoff matrix of the Give-and-Take game.
The matrix is for all the history 1 size strategies.
Usage:
  main_strategy_matrix [OPTIONS]
Available options are:
  --bg N    bonus for giving the resource, default is 0.5
  --cpt N   cost for performing the take action, default is 0.5
  --cst N   cost payed by the subject of the take action, default is 0.5
  -n, --number-stages N  number of stages of the game, default is 1
  -o, --output FILE       name of the file to write the payoff matrix, default is payoff-matrix.csv
  -h, --help              print this help message, and exit
".

/**
 * Return the game parameters specified in the command line.
 */
:- func gameParameters(getopt.option_table(commandLineOptions)) = givetake.parameters.parameters.

gameParameters(Options) = Result :-
	Result^bg = string.det_to_float(getopt.lookup_string_option(Options, bg)),
	Result^cpt = string.det_to_float(getopt.lookup_string_option(Options, cpt)),
	Result^cst = string.det_to_float(getopt.lookup_string_option(Options, cst)),
	Result^numberStages = getopt.lookup_int_option(Options, numberStages)
	.

:- pred createPayoffMatrix(string, givetake.parameters.parameters, io.state, io.state).
:- mode createPayoffMatrix(in, in, di, uo) is cc_multi.

createPayoffMatrix(Filename, GameParameters, !IO) :-
	io.open_output(Filename, IStream, !IO),
	(	% switch
		IStream = ok(Stream),
		io.print(Stream, "\"godel1\",\"godel2\",", !IO),
		io.print(Stream, "\"GT1\",\"GN1\",\"NT1\",\"NNY1\",\"TG1\",\"NG1\",\"TN1\",\"NNN1\",\"FG1\",\"FT1\",", !IO),
		io.print(Stream, "\"GT2\",\"GN2\",\"NT2\",\"NNY2\",\"TG2\",\"NG2\",\"TN2\",\"NNN2\",\"FG2\",\"FT2\",", !IO),
		io.print(Stream, "\"payoff1\",\"payoff2\"\n", !IO),
		solutions.unsorted_aggregate(
			payoffMatrixEntry(GameParameters),
			writePayoffMatrixEntry(Stream),
			!IO),
		io.close_output(Stream, !IO)
	;
		IStream = error(_)
	)
	.

:- pred payoffMatrixEntry(givetake.parameters.parameters, row).
:- mode payoffMatrixEntry(in, out(row)) is nondet.

payoffMatrixEntry(GameParameters, Row) :-
	list.member(Godel1, 0..1023),
	list.member(Godel2, 0..1023),
	givetake.strategy.encodecode.encodecodeHist_det(Strategy1, Godel1, _, _),
	givetake.strategy.encodecode.encodecodeHist_det(Strategy2, Godel2, _, _),
	givetake.game.playAvg(GameParameters, Strategy1, Strategy2, Payoff1, Payoff2, _),
	Row^strategy1 = Strategy1,
	Row^strategy2 = Strategy2,
	Row^payoff1 = Payoff1,
	Row^payoff2 = Payoff2,
	Row^godel1 = Godel1,
	Row^godel2 = Godel2
	.

:- pred writePayoffMatrixEntry(io.output_stream, row, io.state, io.state).
:- mode writePayoffMatrixEntry(in, in(row), di, uo) is det.

writePayoffMatrixEntry(Stream, Row, !IO) :-
	io.print(Stream, rowAsCSV(Row), !IO)
	.

:- func rowAsCSV(row) = string.
:- mode rowAsCSV(in(row)) = out is det.

rowAsCSV(Row) = Result :-
	Row^strategy1 = history(GT1, GN1, NT1, NNY1, TG1, NG1, TN1, NNN1, FG1, FT1),
	Row^strategy2 = history(GT2, GN2, NT2, NNY2, TG2, NG2, TN2, NNN2, FG2, FT2),
	Result = string.format(
		"%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%f,%f\n",
		[
			i(Row^godel1), i(Row^godel2),
			ha(give_take, GT1), ha(give_nonet, GN1), ha(noneg_take, NT1), ha(noneg_nonet, NNY1),
			ha(take_give, TG1), ha(nonet_give, NG1), ha(noneg_take, TN1), ha(nonet_noneg, NNN1),
			fa(yes, FG1), fa(no, FT1),
			ha(give_take, GT2), ha(give_nonet, GN2), ha(noneg_take, NT2), ha(noneg_nonet, NNY2),
			ha(take_give, TG2), ha(nonet_give, NG2), ha(noneg_take, TN2), ha(nonet_noneg, NNN2),
			fa(yes, FG2), fa(no, FT2),
			f(Row^payoff1), f(Row^payoff2)
		 ]
	).

:- func ha(actions, bool) = string.poly_type.

ha(give_take   , yes) = s("\"T\"").
ha(give_take   , no ) = s("\"-\"").
ha(give_nonet  , yes) = s("\"T\"").
ha(give_nonet  , no ) = s("\"-\"").
ha(noneg_take  , yes) = s("\"T\"").
ha(noneg_take  , no ) = s("\"-\"").
ha(noneg_nonet , yes) = s("\"G\"").
ha(noneg_nonet , no ) = s("\"+\"").

ha(take_give   , yes) = s("\"G\"").
ha(take_give   , no ) = s("\"+\"").
ha(nonet_give  , yes) = s("\"G\"").
ha(nonet_give  , no ) = s("\"+\"").
ha(take_noneg  , yes) = s("\"G\"").
ha(take_noneg  , no ) = s("\"+\"").
ha(nonet_noneg , yes) = s("\"T\"").
ha(nonet_noneg , no ) = s("\"-\"").

/**
 * fa(WithResource, GiveOrTakeAction) = Result
 *
 * Convert the action done in the first stage to a string.
 */
:- func fa(bool, bool) = string.poly_type.

fa(yes , yes) = s("\"G\"").
fa(yes , no ) = s("\"+\"").
fa(no  , yes) = s("\"T\"").
fa(no  , no ) = s("\"-\"").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred optionShort(char, commandLineOptions).
:- mode optionShort(in, out) is semidet.
%:- mode optionShort(out, in) is det. % used in debugging: every option should have only one char.

optionShort('b', bg).
optionShort('p', cpt).
optionShort('s', cst).
optionShort('n', numberStages).
optionShort('o', filename).
optionShort('h', help).

:- pred optionLong(string, commandLineOptions).
:- mode optionLong(in, out) is semidet.
%:- mode optionLong(out, in) is det. % used in debugging: every option should have only one string.

optionLong("bg", bg).
optionLong("cpt", cpt).
optionLong("cst", cst).
optionLong("number-stages", numberStages).
optionLong("output", filename).
optionLong("help", help).

:- pred optionDefault(commandLineOptions, getopt.option_data).
:- mode optionDefault(out, out) is multi.
%:- mode optionDefault(in, out) is det. % used in debugging: every option should have a default option.

optionDefault(bg, string("0.5")).
optionDefault(cpt, string("0.5")).
optionDefault(cst, string("0.5")).
optionDefault(numberStages, int(10)).
optionDefault(filename, string("payoff-matrix.csv")).
optionDefault(help, bool(no)).



:- end_module main_strategy_matrix.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
