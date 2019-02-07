colorofcard(red,c(_,S)):-S=hearts;S=diamonds.

colorofcard(black,c(_,S)):-S=clubs;S=spades.

card(c(R,S)):-ranks(LR),suits(LS),member(R,LR),member(S,LS).

% given

ranks([2,3,4,5,6,7,queen,jack,king,ace]).
suits([hearts,diamonds,spades,clubs]).

rankshigher(R1,R2):-
    ranks(R),append(_,[R2|LR],R),member(R1,LR).

samesuit(c(_,S),c(_,S)).

color(hearts,red).
color(diamonds,red).
color(spades,black).
color(clubs,black).

colorcard(C,c(R,S)):-color(S,C),ranks(LR),member(R,LR).

points(ace,11):-!.
points(7,10):-!.
points(king,4):-!.
points(jack,3):-!.
points(queen,2):-!.
points(X,0):-integer(X), X>=2, X=<6.

%Predicado para calcular o número de pontos de uma determinada quantidade de cartas
sum_points([],0).
sum_points([p(_,c(Val,_))|V],N):- points(Val,N2),  sum_points(V,N3) , N is N3 + N2.

%--------------------------------------------------------------------------------
%Predicados para escrita de dados,texto etc.		
%--------------------------------------------------------------------------------
write_rank(ace) :- write("A"),!.
write_rank(king) :- write("K"),!.
write_rank(queen) :- write("Q"),!.
write_rank(jack) :- write("J"),!.
write_rank(X) :- write(X).

render_hands([P1,P2,P3,P4],Trumph,Turn) :- 
    							write(">Trumph : "),render_card(Trumph),
  								render_turn(Turn),
    							writeln("Player 1:"),render_cards(P1),
    							writeln("Player 2:"),render_cards(P2),
    							writeln("Player 3:"),render_cards(P3),
    							writeln("Player 4:"),render_cards(P4),writeln("").

render_cards(L) :- render_hearts(L),
    				render_diamonds(L),
    				render_spades(L),
    				render_clubs(L),writeln(""),!.

render_card(c(R,hearts)) :-write_rank(R),writeln(" ♥  "),!.
render_card(c(R,diamonds)) :-write_rank(R),writeln(" ♦  "),!. 
render_card(c(R,spades)) :-write_rank(R),writeln(" ♠️  "),!.  
render_card(c(R,clubs)) :-write_rank(R),writeln(" ♣  "),!.                       
                                    
render_hearts([]).
render_hearts([c(R,hearts)|L]) :- write_rank(R),write(" ♥ | "),render_hearts(L),!.
render_hearts([_|L]) :- render_hearts(L).

render_diamonds([]).
render_diamonds([c(R,diamonds)|L]) :- write_rank(R),write(" ♦ | "),render_diamonds(L),!.
render_diamonds([_|L]) :- render_diamonds(L).

render_spades([]).
render_spades([c(R,spades)|L]) :-write_rank(R),write(" ♠️ | "),render_spades(L),!.
render_spades([_|L]) :- render_spades(L).

render_clubs([]).
render_clubs([c(R,clubs)|L]) :-write_rank(R),write(" ♣ | "),render_clubs(L),!.
render_clubs([_|L]) :- render_clubs(L).

render_turn(N) :- 
    			 writeln("_______________________________"),
    			 write("Turn "),writeln(N),
    			 writeln("_______________________________").

writeteams :- writeln("-You are in Team 1 together with Player 3-"),
    		  writeln("--- Against Team 2 with Player 2 and 4 ---"),writeln("").
%--------------------------------------------------------------------------------
%--------------------------------------------------------------------------------

%Obter o baralho de cartas "baralhado".
shuffle(K) :- deck(L),random_permutation(L,K).
    
deck(L) :- bagof(X,card(X),L).

%Predicado principal para começar o jogo.
%Obtém o trunfo e baralha as cartas; distribuí as mãos e chama a função que controlará o jogo em execução
play :- writeteams,shuffle([Trumph|Deck]),hand_cards(Deck,[P1,P2,P3,P4],38,Trumph),
    	aux_play([0,0],[P1,P2,P3,P4],[],[],Trumph).

%Predicado que gere o jogo consoante o estado em que se encontra
%Termos:
%( [TURNO_ATUAL , JOGADOR_A_JOGAR] , [MÃOS_JOGADORES] , VAZA_EQUIPA1 , VAZA_EQUIPA2 , TRUNFO]
%
%1ª Cláusula: Jogo encontra-se no turno 10, ou seja, fim de jogo porque nínguem tem cartas. Chama função para calcular o vencedor
%2ª Cláusula: Primeiro turno do jogo. Foi assumido que começa sempre o jogador 1(user) a jogar. 
%			  Calcula as jogadas de cada "bot" chamando a função bots.
%			  Calcula o vencedor da mão, as novas vazas e elimina as cartas das mãos dos jogadores chamando a função handwinners.
%3ª Cláusula: Turno 9. Jogadores só têm uma carta na mão cada logo não ha necessidade de passar pelo processo de calcular as jogadas de cada bot e pedir input ao utilizador.
%		      Limita-se a chamar o predicado que simplesmente joga as cartas de cada um começando pelo que ganhou a mão anterior (lasthandtable)
%4ª Cláusula: Caso geral de obter as jogadas de cada um através do predicado get_table
aux_play([10,_],_,PairOne,PairTwo,_) :- check_winners(PairOne,PairTwo).
aux_play([0,0],[P1,P2,P3,P4],[],[],Trumph) :- render_hands([P1,P2,P3,P4],Trumph,0),
											writeln(""),writeln("Player 1 plays: "),read(P1_Move),
    										get_suit(P1_Move,Suit),member(P1_Move,P1),
    										bots([p(0,P1_Move)],P2,Trumph,Suit,Play2),
    										write("Player 2 plays : "),render_card(Play2),
											append([p(0,P1_Move)],[p(1,Play2)],Table),
											bots(Table,P3,Trumph,Suit,Play3),
    										write("Player 3 plays : "),render_card(Play3),
											append(Table,[p(2,Play3)],NTable),
											bots(NTable,P4,Trumph,Suit,Play4),
    										write("Player 4 plays : "),render_card(Play4),
    										handwinners([p(0,P1_Move),p(1,Play2),p(2,Play3),p(3,Play4)],Trumph,0,[],[],[P1,P2,P3,P4]),!.
aux_play([9,FirstToPlay],[[C1|_],[C2|_],[C3|_],[C4|_]],V1,V2,Trumph):-
    render_hands([[C1|_],[C2|_],[C3|_],[C4|_]],Trumph,9),
    lasthand_table(FirstToPlay,[C1,C2,C3,C4],[],[Trumph,V1,V2]).
aux_play([Turn,FirstToPlay],Hands,V1,V2,Trumph):-
    Turn \= 0,
    render_hands(Hands,Trumph,Turn),
    get_table(FirstToPlay,Hands,[],Trumph,[Turn,V1,V2]).


%Predicado para gerar a "mesa" com as jogadas de cada jogador. 
%Termos:
%( JOGADOR_A_JOGAR , [MÃOS_JOGADORES] , TABLE , TRUNFO [TURNO, VAZA_EQUIPA1 , VAZA_EQUIPA2] )
%
%1ª Cláusula : Já foram geradas as jogadas de cada jogador (encontram-se na lista Table).
%			   Calcula o vencedor da mão, as novas vazas e elimina as cartas das mãos dos jogadores chamando a função handwinners.
%2ª Cláusula : Obter a jogada do utilizador.
%3ª Cláusula : Obter a jogada do bot K.
get_table(_,Hands,Table,Trumph,[Turn,V1,V2]) :- length(Table,4),handwinners(Table,Trumph,Turn,V1,V2,Hands),!.
get_table(0,[P1|Hands],Table,T,Data) :-
    writeln("Player 1 plays: "),read(Y),
    legal(Table,P1,Moves),member(Y,Moves),
    append(Table,[p(0,Y)],NTable),
    get_table(1,[P1|Hands],NTable,T,Data),!.
get_table(K,Hands,[],T,Data) :-
    K \= 0,
    write("Player "),K1 is K+1,write(K1),write(" plays: "),
    nth0(K, Hands, CPU_Hand),
    bots([],CPU_Hand,T,[],Y),
    render_card(Y),np(K,NPlayer),
    get_table(NPlayer,Hands,[p(K,Y)],T,Data),!.
get_table(K,Hands,[p(P,c(R,S))|Table],T,Data) :- 
    K \= 0,
    write("Player "),K1 is K+1,write(K1),write(" plays: "),
    nth0(K, Hands, CPU_Hand),
    bots([p(P,c(R,S))|Table],CPU_Hand,T,S,Y),
    append([p(P,c(R,S))|Table],[p(K,Y)],NTable), render_card(Y),np(K,NPlayer),
    get_table(NPlayer,Hands,NTable,T,Data).

%Predicado para calcular o vencedor de uma mão e "preparar" o jogo para a próxima
%Termos:
%( [TABLE] , TRUNFO , TURNO_ATUAL , VAZA_1 , VAZA_2 , MÃOS_JOGADORES)
handwinners(L,T,Turn,V1,V2,Hands) :- aux_handwinners(L,T,Turn,V1,V2,Hands).

%Através do findall, cria uma lista com o valor de cada jogada na mesa consoante a carta a assistir, se é trunfo ou não, etc.
%A partir dessa lista, o vencedor da mão é aquele com o valor mais alto.
%Uma vez tendo o vencedor da mão, chama a função getnewvazas para acrescentar à vaza da equipa correspondente as cartas ganhas.
%Depois, apaga as cartas das mãos de cada jogador através da função delete_hands e chama de volta o predicado aux_play.
aux_handwinners([p(Pl,c(R,Suit))|Table],Trumph,Turn,V1,V2,Hands) :-
    findall(p(Val,Player),(member(p(Player,X),[p(Pl,c(R,Suit))|Table]),playval(X,Trumph,Suit,Val)),ValList),
    	max_member(p(_,Winner),ValList),
    	getnewvazas([p(Pl,c(R,Suit))|Table],Winner,V1,V2,NV1,NV2),
    	delete_hands([p(Pl,c(R,Suit))|Table],Hands,NHands),NTurn is Turn +1, 
    	aux_play([NTurn,Winner],NHands,NV1,NV2,Trumph).
				

%Predicados para apagar as cartas que foram jogadas das mãos dos jogadores.
delete_hands(Table,Hands,NHands) :- auxdel_hands(Table,Hands,NHands0,0),
                                    auxdel_hands(Table,NHands0,NHands1,1),
                                    auxdel_hands(Table,NHands1,NHands2,2),
                                    auxdel_hands(Table,NHands2,NHands,3).

auxdel_hands([p(0,C)|_],[P1|Hands],[NP1|Hands],0) :- del(P1,C,NP1),!.
auxdel_hands([p(1,C)|_],[P1,P2|Hands],[P1,NP2|Hands],1) :- del(P2,C,NP2),!.
auxdel_hands([p(2,C)|_],[P1,P2,P3,P4],[P1,P2,NP3,P4],2) :- del(P3,C,NP3),!.
auxdel_hands([p(3,C)|_],[P1,P2,P3,P4],[P1,P2,P3,NP4],3) :- del(P4,C,NP4),!.
auxdel_hands([_|R],Hands,K,Player) :- auxdel_hands(R,Hands,K,Player).
    
%Predicado para trabalhar na última mão do jogo
%Termos
%(JOGADOR_A_JOGAR , MÃOS , TABLE , [TRUNFO , VAZA_1 , VAZA_2] )
%
%1ª Cláusula: O predicado já tem as cartas todas jogadas na lista Table e procede a chamar a função handwinners.
%2ª Cláusula: Escrever a jogada do utilizador (não é necessário pedir input pois só existe uma opção.
%3ª Cláusula: Dispor a jogada de cada bot K.
lasthand_table(_,[C1,C2,C3,C4],Table,[Trumph,V1,V2]) :- length(Table,4),handwinners(Table,Trumph,9,V1,V2,[[C1],[C2],[C3],[C4]]),!.
lasthand_table(0,Cards,Table,Data) :- write("Player 1 plays: "), nth0(0, Cards, C), 
    								append(Table,[p(0,C)],NTable),
    								render_card(C),
    								lasthand_table(1,Cards,NTable,Data),!.
lasthand_table(K,Cards,Table,Data) :- 
    				K \= 0,
    				write("Player "),write(K),write(" plays: "),
    				nth0(K, Cards, C), append(Table,[p(K,C)],NTable),np(K,NPlayer),
    				render_card(C),
    				lasthand_table(NPlayer,Cards,NTable,Data).
    
%Apagar um elemento X de uma lista
del([X|L],X,L).
del([Y|L],X,[Y|L2]):-X\==Y,del(L,X,L2).

%Predicado para acrescentar as cartas em Table à vaza correspondente
getnewvazas(Table,Player,V1,V2,NV1,V2) :- 0 is mod(Player,2), append(Table,V1,NV1),writeln(" "),writeln("->Team 1 gets this one!"),writeln(" "),!.
getnewvazas(Table,_,V1,V2,V1,NV2) :- append(Table,V2,NV2),writeln(" "),writeln("->Team 2 gets this one!"),writeln(" ").

%Distribuir as cartas de cada jogador.
hand_cards([C1|Deck],[[C1|P1],P2,P3,P4],NCards,T) :-NCards >= 29,NewNCards is NCards-1,hand_cards(Deck,[P1,P2,P3,P4],NewNCards,T),!.
hand_cards([C1|Deck],[P1,[C1|P2],P3,P4],NCards,T) :-NCards >= 19,NewNCards is NCards-1,hand_cards(Deck,[P1,P2,P3,P4],NewNCards,T),!.
hand_cards([C1|Deck],[P1,P2,[C1|P3],P4],NCards,T) :-NCards >= 9,NewNCards is NCards-1,hand_cards(Deck,[P1,P2,P3,P4],NewNCards,T),!.
hand_cards(Deck,[[],[],[],[T|Deck]],_,T).

%Receber naipe de uma carta
get_suit(c(_,C),C).

%Escolher um elemento aleatório de uma lista
choose([], []).
choose(List, Elt) :-
        length(List, Length), 	  %Comprimento da Lista	
        random(0, Length, Index), %"Index" é um numero aleatório entre 0 e "Length"
        nth0(Index, List, Elt).   %Verdade quando "Elt" é o elemento de índice "Index" em "List"

%Predicado para obter as jogadas legais possíveis tendo em conta as cartas em jogo
%Termos:
%(CARTAS_NA_MESA , MÃO_JOGADOR , JOGADAS_POSSIVEIS)
%1ª Cláusula: Não há cartas em jogo. Pode jogar qualquer coisa.
%2ª Cláusula: Existe pelo menos uma carta na mesa. 
%			  Obter todas as cartas do mesmo naipe.
%3ª Cláusula: Caso não tenha carta para assistir, pode jogar qualquer carta da mão.
legal([],Hand,Hand).
legal([p(_,c(_,S))|_],Hand,Moves) :-findall(c(R1,S),member(c(R1,S),Hand),Moves),Moves \= [],!.
legal(_,Hand,Hand).

%Ordem de jogadores
np(0,1).
np(1,2).
np(2,3).
np(3,0).

%Valor de cada carta. ATENÇÃO: DIFERENTE DOS SEUS PONTOS.
%Serve para determinar o vencedor de uma mão.
card_val(ace,10):-!.
card_val(7,9):-!.
card_val(king,8):-!.
card_val(jack,7):-!.
card_val(queen,6):-!.
card_val(X,Y) :- Y is X-1.

%Obter o valor de cada carta a jogar. 
%De certa forma, a sua utilidade.
%Ex: Com copas a ser trunfo:
%		-Jogar um 6 de paus daria utilidade 5
%		-Jogar um 6 de copas(trunfo) seria 25
playval(c(R,T), c(_,T), _, K) :- card_val(R,K1),K is K1+20,!.
playval(c(R,Suit),_,Suit,K) :- card_val(R,K),!.
playval(_,_,_,0).

playval_aux(c(R,S),c(_,T),K):- S \= T, card_val(R,K),!.

%Obter a carta atual na mesa com o valor mais alto.
%De certa forma, a "vencedora" atual.
max_value([],_,_,_).
max_value(Table,Trumph,Suit,Val) :- findall(V,(member(p(_,X),Table),playval(X,Trumph,Suit,V)),VList),max_member(Val,VList).

%Devolve a carta com maior valor na mão.
max_Hand(Hand,Trumph,K):- setof(h(Val,C),(member(C,Hand), playval_aux(C,Trumph, Val)),ValList), last(ValList,h(_,K)).

%Determina o pensamento básico dos bots.
%Consoante a mesa, este predicado diz que devem jogar a carta miníma que lhes garante ganhar a mão atual (sem ter em conta os próximos a jogar).
greater_minHand(K,c(_,Assist),Trumph,X,Y):- setof(h(Val,C),(member(C,X),playval(C,Trumph,Assist,Val),Val > K),ValList),ValList \= [],first(ValList,h(_,Y)).

first([X|_],X).    

%Função para devolver a jogada de um certo bot.
%1ª Cláusula: Caso a mesa esteja vazia, jogar a maior carta.
%2ª Cláusula: Jogar a carta miníma que lhes garante ganhar a mão atual
%3ª Cláusula: Caso não tenha cartas de maior valor, jogar uma aleatóriamente
bots([],Hand,Trumph,[],Play):- max_Hand(Hand,Trumph,Play).
bots([p(_,T)|Table],Hand,Trumph,Suit,Play) :- legal([p(_,T)|Table],Hand,X),max_value([p(_,T)|Table],Trumph,Suit,K),greater_minHand(K,T,Trumph,X,Play),!.
bots([p(_,T)|Table],Hand,_,_,Play) :- legal([p(_,T)|Table],Hand,X),choose(X,Play).

%Verificar quem ganhou o jogo e escrever no ecrã.
check_winners(X,Y) :- sum_points(X,Team1Points),sum_points(Y,Team1Points),
    				writeln("______________________________________________________"),
    				writeln("_________________________DRAW_________________________"),
    				writeln("______________________________________________________").
check_winners(X,Y) :- sum_points(X,Team1Points),sum_points(Y,Team2Points),Team1Points > Team2Points,
    					writeln("_______________________________________________________"),
    					writeln("______________________TEAM 1 WINS______________________"),
    					writeln("_______________________________________________________"),!.
check_winners(_,_) :-
    					writeln("_______________________________________________________"),
    					writeln("______________________TEAM 2 WINS______________________"),
    					writeln("_______________________________________________________").
