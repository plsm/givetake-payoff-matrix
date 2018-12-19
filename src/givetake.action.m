/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2012/12/31
 */
:- module givetake.action.

:- interface.

:- import_module givetake.parameters, givetake.player.
:- import_module io, list.

/**
  * Provides the actions available in a Give-Take game.
  */
:- type action --->
	noneg ;
	give ;
	nonet ;
	take.

/**
 * Represents the possible combinations of actions in each stage of the Give-Take game.
 */
:- type actions --->
	give_take ;
	give_nonet ;
	noneg_take ;
	noneg_nonet ;
	take_give ;
	nonet_give ;
	take_noneg ;
	nonet_noneg.

:- pred valid(action, action, player, actions).
:- mode valid(in, in, in, out) is semidet.
:- mode valid(in, in, out, out) is semidet.
:- mode valid(out, out, in, in) is semidet.
:- mode valid(out, out, out, in) is det.
:- mode valid(out, out, in, out) is multi.

:- func actions(action, action) = actions.

:- pred value(parameters, actions, float, float).
:- mode value(in, in, out, out) is det.

:- func toString(action) = string.

:- func toString(player, actions) = string.

:- func toStringActionSequence(player, list(actions)) = string.

:- pred printActionSequence(io.output_stream, list(actions), io, io).
:- mode printActionSequence(in, in, di, uo) is det.

/**
 * Return the action in the actions pair of the given player.
 */
:- func action(player, actions) = action.

/**
 * swapResource(ActionsSequences, SwapActions, RestActionsSequences)

 * Unify {@code SwapActions} with the first pair of actions that swap the
 * resource possession.
  
 */
:- pred swapResource(list(actions), actions, list(actions)).
:- mode swapResource(in, out, out) is semidet.

:- implementation.

:- import_module exception, float, string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

valid(give,  take,  fst, give_take).
valid(give,  nonet, fst, give_nonet).
valid(noneg, take,  fst, noneg_take).
valid(noneg, nonet, fst, noneg_nonet).

valid(take,  give,  snd, take_give).
valid(nonet, give,  snd, nonet_give).
valid(take,  noneg, snd, take_noneg).
valid(nonet, noneg, snd, nonet_noneg).

actions(A1, A2) = AS :-
	(if
		valid(A1, A2, _, R)
	then
		AS = R
	else
		throw("givetake.action.actions/2: Invalid parameters")
	).

value(Parameters,  give_take,   Parameters^bg - Parameters^cst, 1.0 - Parameters^cpt).
value(Parameters,  give_nonet,  Parameters^bg,                  1.0).
value(Parameters,  noneg_take,  - Parameters^cst,               1.0 - Parameters^cpt).
value(_Parameters, noneg_nonet, 1.0,                            0.0).
value(Parameters,  take_give,   1.0 - Parameters^cpt,           Parameters^bg - Parameters^cst).
value(Parameters,  nonet_give,  1.0,                            Parameters^bg).
value(Parameters,  take_noneg,  1.0 - Parameters^cpt,           - Parameters^cst).
value(_Parameters, nonet_noneg, 0.0,                            1.0).

toString(give)  = "G".
toString(take)  = "T".
toString(noneg) = "+".
toString(nonet) = "-".

toString(Player, Actions) = Result :-
	valid(ActionFst, ActionSnd, _, Actions),
	(
		Player = fst,
		Result = toString(ActionFst)
		;
		Player = snd,
		Result = toString(ActionSnd)
	).

toStringActionSequence(Player, ActionSequence) = Result :-
	ActionSequence = [],
	Result = ""
	;
	ActionSequence = [Actions | RestActionSequence],
	Result = string.format("%s%s", [s(toString(Player, Actions)), s(toStringActionSequence(Player, RestActionSequence))])
	.

printActionSequence(Stream, ActionSequence, !IO) :-
	io.print(Stream, toStringActionSequence(fst, ActionSequence), !IO),
	io.nl(Stream, !IO),
	io.print(Stream, toStringActionSequence(snd, ActionSequence), !IO),
	io.nl(Stream, !IO).

action(fst, give_take)   = give.
action(fst, noneg_take)  = noneg.
action(fst, take_give)   = take.
action(fst, nonet_give)  = nonet.
action(fst, give_nonet)  = give.
action(fst, noneg_nonet) = noneg.
action(fst, take_noneg)  = take.
action(fst, nonet_noneg) = nonet.

action(snd, give_take)   = take.
action(snd, noneg_take)  = take.
action(snd, take_give)   = give.
action(snd, nonet_give)  = give.
action(snd, give_nonet)  = nonet.
action(snd, noneg_nonet) = nonet.
action(snd, take_noneg)  = noneg.
action(snd, nonet_noneg) = noneg.

swapResource([Actions | RestActionsSequences], SwapActions, Result) :-
	(if
		Actions \= noneg_nonet,
		Actions \= nonet_noneg
	then
		SwapActions = Actions,
		Result = RestActionsSequences
	else
		swapResource(RestActionsSequences, SwapActions, Result)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module givetake.action.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
