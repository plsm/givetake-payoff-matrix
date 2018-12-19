/**
 * This module provides types, predicates and functions to handle
 * Give-and-Take strategies.  There are strategies that have a history of
 * size 1, and a strategy that can give or take the resource depending on
 * how long it has the resource, or how long it does not have it.

 * @author Pedro Mariano
 * @version 1.0 2018/12/14
 */
:- module givetake.strategy.

:- interface.

:- include_module encodecode.
:- import_module bool.

/**
 * The Give-and-Take strategies.
 */
:- type strategy --->
	/**
	 * history(GT, GN, NT, NNY, TG, NG, TN, NNN, FG, FT)
	 *
	 * Strategy with an history of size 1.
	 */
	history(gt::bool, gn::bool, nt::bool, nny::bool, tg::bool, ng::bool, tn::bool, nnn::bool, fg::bool, ft::bool) ;

	/**
	 * time(TGI, TTA)
	 *
	 * Strategy that gives the resource if it has it for {@code TGI} stages,
	 * and takes it if it does not have the resource for {@code TTA} stages.
	 */
	time(tgi::int, tta::int).

:- inst hist == bound(history(
	ground, ground, ground, ground,
	ground, ground, ground, ground,
	ground, ground)).

:- implementation.

:- import_module maybe.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module givetake.strategy.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
