/**
 * This module provides a predicate to play a Give-and-Take game.

 * @author Pedro Mariano
 * @version 1.0 2013/01/15
 */
:- module givetake.game.

:- interface.

:- import_module givetake.action, givetake.parameters, givetake.strategy.
:- import_module io, list.

/**
 * play(Game, StrategyFst, StrategySnd, PayoffFst, PayoffSnd)

 * The first player starts with the resource.

 * <p> Computes the exact payoff for deterministic strategies. For
 * stochastic strategies the average payoff is returned.
 */
:- pred playAvg(givetake.parameters.parameters, givetake.strategy.strategy, givetake.strategy.strategy, float, float, list(actions)).
:- mode playAvg(in, in, in, out, out, out) is det.

%:- pred playMemoTableReset(io, io).
%:- mode playMemoTableReset(di, uo) is det.

%:- pred playMemoTableStatistics(io, io).
%:- mode playMemoTableStatistics(di, uo) is det.

:- implementation.

:- import_module givetake.player.
:- import_module bool, exception, int, float, table_statistics.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

playAvg(Parameters, StrategyFst, StrategySnd, PayoffFst, PayoffSnd, ActionSequence) :-
	playDebugMemo(
		Parameters, Parameters^numberStages,
		StrategyFst, StrategySnd, fst,
		PayoffFst, PayoffSnd, ActionSequence
	).

%playMemoTableReset(!IO) :-
%	table_reset_for_playDebugMemo_8(!IO).

%playMemoTableStatistics(!IO) :-
%	table_statistics_for_playDebugMemo_8(Statistics, !IO),
%	table_statistics.write_table_stats(Statistics^call_table_stats^current_stats, !IO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- pragma memo(playDebugMemo/8, [
		specified([promise_implied, promise_implied, addr, addr, value, output, output, output])]).

:- pred playDebugMemo(
	givetake.parameters.parameters, int,
	givetake.strategy.strategy, givetake.strategy.strategy, player,
	float, float, list(actions)).
:- mode playDebugMemo(in, in,   in, in,  in,  out, out, out) is det.

playDebugMemo(Parameters, Time, StrategyFst, StrategySnd, PlayerWithResource, PayoffFst, PayoffSnd, ActionSequence) :-
	playDebug(Parameters, Time, StrategyFst, StrategySnd, PlayerWithResource, [], 0, PayoffFst, PayoffSnd, ActionSequence).

:- pred playDebug(givetake.parameters.parameters, int, givetake.strategy.strategy, givetake.strategy.strategy, player, list(actions), int, float, float, list(actions)).
:- mode playDebug(in, in,  in, in,  in, in, in,  out, out, out) is det.

playDebug(Parameters, Time, StrategyFst, StrategySnd, PlayerWithResource, History, ResourceExchange, PayoffFst, PayoffSnd, ActionSequence) :-
	(if
		Time = 0
	then
		PayoffFst = 0.0,
		PayoffSnd = 0.0,
		ActionSequence = []
	else
		ActionFst = action(StrategyFst, fst, PlayerWithResource, History, ResourceExchange),
		ActionSnd = action(StrategySnd, snd, PlayerWithResource, History, ResourceExchange),
		(if
			givetake.action.valid(ActionFst, ActionSnd, PlayerWithResource, AS)
		then
			Actions = AS
		else
			throw("givetake.game.play/9: the action computed by function action/5 is invalid")
		),
		(if
			Actions = noneg_nonet
			;
			Actions = nonet_noneg
		then
			NextPlayerWithResource = PlayerWithResource,
			NextResourceExchange = ResourceExchange + 1
		else
			NextPlayerWithResource = givetake.player.partner(PlayerWithResource),
			NextResourceExchange = 0
		),
		NextHistory = [Actions | History],
		playDebug(
			Parameters, Time - 1,
			StrategyFst, StrategySnd,
			NextPlayerWithResource, NextHistory, NextResourceExchange,
			RestPayoffFst, RestPayoffSnd, RestActionSequence),
		givetake.action.value(Parameters, Actions, RoundPayoffFst, RoundPayoffSnd),
		PayoffFst = RoundPayoffFst + RestPayoffFst,
		PayoffSnd = RoundPayoffSnd + RestPayoffSnd,
		ActionSequence = [Actions | RestActionSequence]
	).

/**
 * Computes the action that the strategy is going to do, given if it is the
 * first or second player, who has the resource, the game action history,
 * and how far back was the last resource exchange.
 */
:- func action(givetake.strategy.strategy, player, player, list(actions), int) = action.

action(history(GT, GN, NT, NNy, TG, NG, TN, NNn, FG, FT), WhoIAm, PlayerWithResource, ActionSequence, _ResourceExchange) = Result :-
	ActionSequence = [LastActions | _],
	lookup(GT, GN, NT, NNy, TG, NG, TN, NNn, WhoIAm, LastActions) = Result
	;
	ActionSequence = [],
	(if
		PlayerWithResource = WhoIAm
	then
		FG = yes,
		Result = give
		;
		FG = no,
		Result = noneg
	else
		FT = yes,
		Result = take
		;
		FT = no,
		Result = nonet
	).

action(time(TG, TT), WhoIAm, PlayerWithResource, _ActionSequence, ResourceExchange) = Result :-
	(if
		WhoIAm = PlayerWithResource
	then
		(if
			ResourceExchange >= TG
		then
			Result = give
		else
			Result = noneg
		)
	else
		(if
			ResourceExchange >= TT
		then
			Result = take
		else
			Result = nonet
		)
	).

:- func lookup(bool, bool, bool, bool, bool, bool, bool, bool, player, actions) = action.

lookup(yes, _GN, _NT, _NNy, _TG, _NG, _TN, _NNn, fst, give_take)   = take.
lookup(no,  _GN, _NT, _NNy, _TG, _NG, _TN, _NNn, fst, give_take)   = nonet.
lookup(_GT, yes, _NT, _NNy, _TG, _NG, _TN, _NNn, fst, give_nonet)  = take.
lookup(_GT, no,  _NT, _NNy, _TG, _NG, _TN, _NNn, fst, give_nonet)  = nonet.
lookup(_GT, _GN, yes, _NNy, _TG, _NG, _TN, _NNn, fst, noneg_take)  = take.
lookup(_GT, _GN, no,  _NNy, _TG, _NG, _TN, _NNn, fst, noneg_take)  = nonet.
lookup(_GT, _GN, _NT, yes,  _TG, _NG, _TN, _NNn, fst, noneg_nonet) = give.
lookup(_GT, _GN, _NT, no,   _TG, _NG, _TN, _NNn, fst, noneg_nonet) = noneg.

lookup(_GT, _GN, _NT, _NNy, yes, _NG, _TN, _NNn, fst, take_give)   = give.
lookup(_GT, _GN, _NT, _NNy, no,  _NG, _TN, _NNn, fst, take_give)   = noneg.
lookup(_GT, _GN, _NT, _NNy, _TG, yes, _TN, _NNn, fst, nonet_give)  = give.
lookup(_GT, _GN, _NT, _NNy, _TG, no,  _TN, _NNn, fst, nonet_give)  = noneg.
lookup(_GT, _GN, _NT, _NNy, _TG, _NG, yes, _NNn, fst, take_noneg)  = give.
lookup(_GT, _GN, _NT, _NNy, _TG, _NG, no,  _NNn, fst, take_noneg)  = noneg.
lookup(_GT, _GN, _NT, _NNy, _TG, _NG, _TN, yes,  fst, nonet_noneg) = take.
lookup(_GT, _GN, _NT, _NNy, _TG, _NG, _TN, no,   fst, nonet_noneg) = nonet.

lookup(_GT, _GN, _NT, _NNy, yes, _NG, _TN, _NNn, snd, give_take)   = give.
lookup(_GT, _GN, _NT, _NNy, no,  _NG, _TN, _NNn, snd, give_take)   = noneg.
lookup(_GT, _GN, _NT, _NNy, _TG, yes, _TN, _NNn, snd, give_nonet)  = give.
lookup(_GT, _GN, _NT, _NNy, _TG, no,  _TN, _NNn, snd, give_nonet)  = noneg.
lookup(_GT, _GN, _NT, _NNy, _TG, _NG, yes, _NNn, snd, noneg_take)  = give.
lookup(_GT, _GN, _NT, _NNy, _TG, _NG, no,  _NNn, snd, noneg_take)  = noneg.
lookup(_GT, _GN, _NT, _NNy, _TG, _NG, _TN, yes,  snd, noneg_nonet) = take.
lookup(_GT, _GN, _NT, _NNy, _TG, _NG, _TN, no,   snd, noneg_nonet) = nonet.

lookup(yes, _GN, _NT, _NNy, _TG, _NG, _TN, _NNn, snd, take_give)   = take.
lookup(no,  _GN, _NT, _NNy, _TG, _NG, _TN, _NNn, snd, take_give)   = nonet.
lookup(_GT, yes, _NT, _NNy, _TG, _NG, _TN, _NNn, snd, nonet_give)  = take.
lookup(_GT, no,  _NT, _NNy, _TG, _NG, _TN, _NNn, snd, nonet_give)  = nonet.
lookup(_GT, _GN, yes, _NNy, _TG, _NG, _TN, _NNn, snd, take_noneg)  = take.
lookup(_GT, _GN, no,  _NNy, _TG, _NG, _TN, _NNn, snd, take_noneg)  = nonet.
lookup(_GT, _GN, _NT, yes,  _TG, _NG, _TN, _NNn, snd, nonet_noneg) = give.
lookup(_GT, _GN, _NT, no,   _TG, _NG, _TN, _NNn, snd, nonet_noneg) = noneg.

:- end_module givetake.game.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
