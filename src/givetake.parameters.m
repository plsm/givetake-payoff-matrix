/**
 * This module provides the definition of the Give-and-Take parameters.
 * They are:
 *
 * 1) the bonus for giving the resource;
 *
 * 2) the cost of performing the take action;
 *
 * 3) the cost the subject of the take action has to pay;
 *
 * 4) the number of iterations of the game.

 * @author Pedro Mariano
 * @version 1.0 2018/12/14
 */
:- module givetake.parameters.

:- interface.

:- type parameters --->
	parameters(
		bg::float,
		cpt::float,
		cst::float
	).

/**
 * True if the game parameters are valid.
 */
:- pred valid(parameters).
:- mode valid(in) is semidet.

:- implementation.

:- import_module float, int.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

valid(Parameters) :-
	Parameters^bg < 1.0,
	Parameters^bg >= 0.0,
	Parameters^cpt > 0.0,
	Parameters^cst >= 0.0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module givetake.parameters.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
