/**
 * Provides a type to represent players in Give-Take.

 * @author Pedro Mariano
 * @version 1.0 2013/01/ 3
 */
:- module givetake.player.

:- interface.

:- import_module bool.

:- type player --->
	fst ;
	snd.

:- func partner(player) = player.

:- pred bool(player, bool).
:- mode bool(in, out) is det.
:- mode bool(out, in) is det.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions
:- func label(player) = int.

label(fst) = 1.
label(snd) = 2.

partner(fst) = snd.
partner(snd) = fst.

bool(fst, yes).
bool(snd, no).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module givetake.player.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
