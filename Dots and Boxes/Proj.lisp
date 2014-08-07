;;;;--------------------------------------------------------------
;;;; Grupo 10
;;;;  Andre Pires   - 68593
;;;;  Filipe Bento  - 71037
;;;;  Joana Condeco - 68624
;;;;
;;;;  Tecnico Lisboa - 2013
;;;;--------------------------------------------------------------


;;;;--------------------------------------------------------------
;;;;                    Moedas e Fios
;;;;--------------------------------------------------------------


;;;;--------------------------------------------------------------
;;;; Tipo Posicao


;;; cria-posicao: int x int --> posicao
;;; Retorna uma posicao
(defun cria-posicao (linha coluna)
	(cons linha coluna))

;;; posicao-linha: posicao --> int
;;; Retorna a linha da posicao
(defun posicao-linha (pos)
	(car pos))

;;; posicao-coluna: posicao --> int
;;; Retorna a coluna da posicao
(defun posicao-coluna (pos)
	(cdr pos))

;;; posicao-p: universal --> logico
;;; Verifica se o valor recebido e uma posicao.
;;; Retorna T caso seja uma posicao; NIL em caso contrario.
(defun posicao-p (pos)
	(and (consp pos)
		 (integerp (car pos))
		 (integerp (cdr pos))
		 (>= (car pos) 0)
		 (>= (cdr pos) 0)))

;;; posicoes-iguais-p: posicao x posicao --> logico
;;; Verifica se duas posicoes sao iguais.
;;; Retorna T caso sejam iguais; NIL em caso contrario.
(defun posicoes-iguais-p (pos1 pos2)
	(and (= (car pos1) (car pos2)) 
		 (= (cdr pos1) (cdr pos2))))

;;;;--------------------------------------------------------------
;;;; Tipo Fio

;;; cria-fio: int x posicao x posicao --> fio
;;; Retorna um fio.
(defun cria-fio (id pos1 pos2)
	(list id pos1 pos2))

;;; fio-id: fio --> int
;;; Retorna o identificador do fio
(defun fio-id (fio)
	(car fio))

;;; fio-origem: fio --> posicao
;;; Retorna a posicao de origem do fio
(defun fio-origem (fio)
	(nth 1 fio))

;;; fio-destino: fio --> posicao
;;; Retorna a posicao de destino do fio
(defun fio-destino (fio)
	(nth 2 fio))

;;; fio-p: universal --> logico
;;; Verifica se o valor recebido e um fio.
;;; Retorna T caso seja; NIL em caso contrario.
(defun fio-p (obj)
	(and (listp obj)
		 (= (length obj) 3)
		 (> (car obj) 0)
		 (posicao-p (nth 1 obj))
		 (posicao-p (nth 2 obj))))

;;;;--------------------------------------------------------------
;;;; Tipo Moeda


;;; Implementado o tipo moeda com funcoes basicas sobre manipulacao de moedas
;;;  por forma a simplificar e abstrair o restante codigo.


;;; cria-moeda: posicao x int --> moeda
;;; Retorna uma nova moeda.
(defun cria-moeda (pos valor)
	(list pos valor))

;;; moeda-posicao: moeda --> posicao
;;; Retorna a posicao associada a moeda.
(defun moeda-posicao (moeda)
	(car moeda))

;;; moeda-valor: moeda --> int positivo
;;;Retorna o valor da moeda.
(defun moeda-valor (moeda)
	(second moeda))

;;; moeda-p: universal --> logico
;;; Verifica se o valor recebido e uma moeda.
;;; Retorna T caso seja; NIL em caso contrario.
(defun moeda-p (moeda)
	(and (consp moeda)
		 (posicao-p (moeda-posicao moeda))
		 (integerp (moeda-valor moeda))
		 (> (moeda-valor moeda) 0)
		 (< (moeda-valor moeda) 10)))

;;; set-moeda-valor!: moeda x int positivo --> {}
;;; Altera o valor de uma moeda para o valor recebido.
;;; Nao retorna nada.
(defun set-moeda-valor! (moeda valor)
	(setf (second moeda) valor))

;;;;--------------------------------------------------------------
;;;; Tipo Tabuleiro


(defstruct Tabuleiro
	numlinhas        ;numero de linhas do tabuleiro
	numcols          ;numero de colunas do tabuleiro
	(lstmoedas '())  ;lista de moedas em jogo
	(lstfios   '())  ;lista de fios em jogo
	(somamoedas 0)   ;soma total do valor das moedas em jogo
	(novoid 0))      ;identificador do proximo fio a ser acrescentado


;;; cria-tabuleiro: int (>= 2) x int (>= 2) --> tabuleiro
;;; Retorna um novo tabuleiro (vazio) com o numero de linhas e colunas recebido.
(defun cria-tabuleiro (numl numc)
	(make-Tabuleiro :numlinhas numl :numcols numc))

;;; copia-tabuleiro: tabuleiro --> tabuleiro
;;; Retorna uma copia (deep copy) do tabuleiro recebido.
(defun copia-tabuleiro (tabuleiro)
	(make-Tabuleiro :numlinhas  (Tabuleiro-numlinhas tabuleiro)
				    :numcols    (Tabuleiro-numcols tabuleiro)
				    :lstmoedas  (copy-list (Tabuleiro-lstmoedas tabuleiro))
				    :lstfios    (copy-list (Tabuleiro-lstfios tabuleiro))
				    :somamoedas (Tabuleiro-somamoedas tabuleiro)
				    :novoid     (Tabuleiro-novoid tabuleiro)))

;;; tabuleiro-linhas: tabuleiro --> int positivo (>= 2)
;;; Retorna o numero de linhas do tabuleiro.
(defun tabuleiro-linhas (tabuleiro)
	(Tabuleiro-numlinhas tabuleiro))

;;; tabuleiro-colunas: tabuleiro --> int positivo (>= 2)
;;; Retorna o numero de colunas do tabuleiro.
(defun tabuleiro-colunas (tabuleiro)
	(Tabuleiro-numcols tabuleiro))

;;; tabuleiro-fios: tabuleiro --> lista
;;; Retorna uma lista com os fios existentes no tabuleiro.
(defun tabuleiro-fios (tabuleiro)
	(Tabuleiro-lstfios tabuleiro))

;;; tabuleiro-fio-com-id: tabuleiro x int positivo --> fio
;;; Retorna o fio com o id correspondente.
(defun tabuleiro-fio-com-id (tabuleiro id)
	(let ((listafios (Tabuleiro-lstfios tabuleiro)))
		(dolist (el listafios)
			(when (equal (fio-id el) id)
				(return el)))))

;;; tabuleiro-fios-posicao: tabuleiro x posicao --> lista
;;; retorna uma lista com os fios que estao ligados 'a posicao dada.
(defun tabuleiro-fios-posicao (tabuleiro pos)
	(let ((listafios (Tabuleiro-lstfios tabuleiro))
		  (novalista '()))
		(dolist (el listafios)
			(when (or (posicoes-iguais-p (fio-origem el) pos)
					  (posicoes-iguais-p (fio-destino el) pos))
				(push el novalista)))
		novalista))

;;; tabuleiro-moeda-posicao: tabuleiro x posicao --> int positivo
;;; Retorna o valor da moeda associada a posicao recebida.
(defun tabuleiro-moeda-posicao (tabuleiro pos)
	(moeda-valor (tabuleiro-moeda tabuleiro pos)))

;;; Funcao auxiliar
;;; tabuleiro-moeda: tabuleiro x posicao --> moeda
;;; Retorna a moeda com a posicao indicada.
(defun tabuleiro-moeda (tabuleiro pos)
	(let ((listamoedas (Tabuleiro-lstmoedas tabuleiro)))
		(dolist (el listamoedas)
			(when (equal (moeda-posicao el) pos)
				(return el)))))

;;; tabuleiro-total-moedas: tabuleiro --> int
;;; Retorna a soma do valor total de moedas em jogo; caso nao hajam moedas, retorna 0.
(defun tabuleiro-total-moedas (tabuleiro)
	(Tabuleiro-somamoedas tabuleiro))

;;; tabuleiro-adiciona-fio!: tabuleiro x posicao x posicao --> {}
;;; Adiciona aos fios do tabuleiro um novo fio que liga as duas posicoes dadas.
;;; Nao retorna nada. 
(defun tabuleiro-adiciona-fio! (tabuleiro pos1 pos2)
	(incf (Tabuleiro-novoid tabuleiro))
	(push (cria-fio (Tabuleiro-novoid tabuleiro) pos1 pos2)
		(Tabuleiro-lstfios tabuleiro)))

;;; tabuleiro-adiciona-moeda-posicao!: tabuleiro x posicao x int positivo --> {}
;;; Adiciona uma moeda a lista de moedas existentes. Caso ja exista 
;;;  uma moeda com essa posicao, essa moeda passa a ter o valor recebido.
;;; Nao retorna nada.
(defun tabuleiro-adiciona-moeda-posicao! (tabuleiro pos valor)
	(let ((listamoedas (Tabuleiro-lstmoedas tabuleiro)))
		(dolist (el listamoedas)
			(when (posicoes-iguais-p (moeda-posicao el) pos)
				(progn
					(incf (Tabuleiro-somamoedas tabuleiro) (- valor (moeda-valor el)))
					(set-moeda-valor! el valor)
					(return-from tabuleiro-adiciona-moeda-posicao!))))
		(push (cria-moeda pos valor) (Tabuleiro-lstmoedas tabuleiro))
		(incf (Tabuleiro-somamoedas tabuleiro) valor)))

;;; tabuleiro-remove-fio-com-id!: tabuleiro x int --> {}
;;; Remove o fio com o identificador recebido do tabuleiro. Caso o fio nao exista,
;;; nao faz nada e o tabuleiro mantem-se inalterado.
;;; Nao retorna nada.
(defun tabuleiro-remove-fio-com-id! (tabuleiro id)
	(let ((listafios (Tabuleiro-lstfios tabuleiro)))
		(if (eq (fio-id (first listafios)) id)
			(setf (Tabuleiro-lstfios tabuleiro) (rest listafios))
			(delete (tabuleiro-fio-com-id tabuleiro id) listafios))))

;;; tabuleiro-remove-moeda-posicao!: tabuleiro x posicao --> {}
;;; Remove do tabuleiro a moeda associada a posicao recebida. Caso a moeda nao exista,
;;;  a funcao nao faz nada e o tabuleiro nao e alterado.
;;; Nao retorna nada.
(defun tabuleiro-remove-moeda-posicao! (tabuleiro pos)
	(let ((listamoedas (Tabuleiro-lstmoedas tabuleiro)))
		(when (posicoes-iguais-p (moeda-posicao (first listamoedas)) pos)
 			(setf (Tabuleiro-lstmoedas tabuleiro) (rest listamoedas))
 			(return-from tabuleiro-remove-moeda-posicao!))
		
		(dolist (el listamoedas)
			(when (posicoes-iguais-p (moeda-posicao el) pos)
				(progn
					(decf (Tabuleiro-somamoedas tabuleiro) (moeda-valor el))
					(delete el listamoedas))))))


;;;;--------------------------------------------------------------
;;;; Tipo Jogo

(defstruct Jogo
	tab 					;Tabuleiro de jogo
	(pntsjog1 0)			;Pontuacao do jogador 1
	(pntsjog2 0)			;Pontuacao do jogador 2
	(proximo  1)			;Proximo jogador a jogar
	(fiosremovidos '()))	;Fios removidos do jogo

;;; cria-jogo: tabuleiro --> jogo
;;; Cria um jogo a partir de um tabuleiro recebido. Os restantes valores do jogo 
;;; vem com o seu valor inicial.
(defun cria-jogo (tabuleiro)
	(make-Jogo :tab tabuleiro))

;;; copia-jogo: jogo --> jogo
;;; Recebe um jogo e devolve uma deep copy do jogo.
(defun copia-jogo (jogo)
	(make-Jogo :tab (copia-tabuleiro (Jogo-tab jogo))
			   :pntsjog1 (Jogo-pntsjog1 jogo)
			   :pntsjog2 (Jogo-pntsjog2 jogo)
			   :proximo  (Jogo-proximo jogo)
 			   :fiosremovidos (copy-list (Jogo-fiosremovidos jogo))))

;;; jogo-tabuleiro: jogo --> tabuleiro
;;; Retorna o tabuleiro de jogo.
(defun jogo-tabuleiro (jogo)
	(Jogo-tab jogo))

;;; jogo-jogador: jogo --> int
;;; Retorna o ID do proximo jogador a jogar.
(defun jogo-jogador (jogo)
	(Jogo-proximo jogo))

;;;  jogo-pontos-jogador1: jogo --> int
;;; Retorna os pontos do jogador 1.
(defun jogo-pontos-jogador1 (jogo)
	(Jogo-pntsjog1 jogo))

;;; jogo-pontos-jogador2: jogo --> int
;;; Retorna os pontos do jogador 2.
(defun jogo-pontos-jogador2 (jogo)
	(Jogo-pntsjog2 jogo))

;;; jogo-historico-jogadas: jogo --> lista
;;; Retorna uma lista com os ids dos fios removidos por ordem cronologica.
(defun jogo-historico-jogadas (jogo)
	(let ((listid '())
		  (removidos (Jogo-fiosremovidos jogo)))
		  (dolist (el removidos)
		  	(push (fio-id el) listid))
		  listid))

;;; troca-jogador: jogo --> {}
;;; Altera o jogador actual do jogo para o proximo jogador.
(defun troca-jogador (jogo)
	(if (eq (Jogo-proximo jogo) 1) 
		(setf (Jogo-proximo jogo) 2)
		(setf (Jogo-proximo jogo) 1)))

;;; jogo-aplica-jogada: fio x inteiro --> {}		
;;; Recebe um fio, um inteiro com o id e aplica a jogada ao fio com o id dado. 
;;; Remove o fio de jogo e as possiveis moedas que tenham ficado sem fios, 
;;;  somando o seu valor a pontuacao do jogador q esta' a jogar.
;;; Actualiza o valor total das moedas na mesa.
;;; Nao retorna nada.
(defun jogo-aplica-jogada! (jogo id)
	(let ((fio (tabuleiro-fio-com-id (Jogo-tab jogo) id))
		  (valormoedas 0))
		(when (not fio)
            (return-from jogo-aplica-jogada! NIL))

		;; Guarda o fio nos removidos e apaga-o da lista de fios
        (push fio (Jogo-fiosremovidos jogo))
        (tabuleiro-remove-fio-com-id! (Jogo-tab jogo) id)

		;; Caso a moeda na pos de origem nao tenha mais fios ligados,
		;; remove-a e guarda o seu valor
		(when (eq (tabuleiro-fios-posicao (Jogo-tab jogo) (fio-origem fio)) '())
			(incf valormoedas (tabuleiro-moeda-posicao (Jogo-tab jogo) (fio-origem fio)))
			(tabuleiro-remove-moeda-posicao! (Jogo-tab jogo) (fio-origem fio)))

		;; Caso a moeda na pos de destino nao tenha mais fios ligados, 
		;; remove-a e guarda o seu valor
		(when (eq (tabuleiro-fios-posicao (Jogo-tab jogo) (fio-destino fio)) '())
			(incf valormoedas (tabuleiro-moeda-posicao (Jogo-tab jogo) (fio-destino fio)))
			(tabuleiro-remove-moeda-posicao! (Jogo-tab jogo) (fio-destino fio)))
		
		
		;; Cortou um fio sem moedas para retirar
		(if (= valormoedas 0)
			(troca-jogador jogo)
			;; Havendo moedas para retirar, adiciona o seu valor 'a pontuacao do jogador
			;; que fez a jogada
			(if (eq (Jogo-proximo jogo) 1)
				(incf (Jogo-pntsjog1 jogo) valormoedas)
				(incf (Jogo-pntsjog2 jogo) valormoedas)))))

;;; jogo-terminado-p: jogo --> logico
;;; Verifica se um jogo esta' terminado.
(defun jogo-terminado-p (jogo)
	(if (eq (tabuleiro-fios (Jogo-tab jogo))  '())
		T
		NIL))


;;;;--------------------------------------------------------------
;;;; Tipo Problema

(defstruct Problema
	estado-inicial
	jogador
	accoes       
	resultado       
	teste-corte-p
	funcao-avaliacao
	historico-accoes
	chave-equivalencia)

;;; jogador: jogo --> int
;;; Recebe um jogo e devolve o ID do proximo jogador a jogar.
(defun jogador (jogo)
	(jogo-jogador jogo))

;;; accoes: jogo --> lista
;;; Retorna a lista de fios do tabuleiro por ordem decrescente.
(defun accoes (jogo)
	(let ((lista '()))
		(dolist (el (tabuleiro-fios (jogo-tabuleiro jogo)))
			(push (fio-id el) lista))
		(sort lista #'>)))

;;; resultado: jogo x int --> jogo
;;; Retorna uma novo jogo com a jogada aplicada. Nao altera o tabuleiro do jogo passado.
(defun resultado (jogo id)
	(let ((jogo2 (copia-jogo jogo)))
		(jogo-aplica-jogada! jogo2 id)
		jogo2))

;;; teste-terminal-p: jogo x int --> logico
;;; Verifica se um jogo e terminal ou nao. Um jogo e' terminal caso nao haja 
;;;  mais fios no tabuleiro ou caso tenha atingido a profundidade maxima que o algoritmo
;;;  minimax permite.
(defun teste-terminal-p (jogo depth)
	(declare (ignore depth))
		(jogo-terminado-p jogo))

;;; utilidade: jogo x int --> int
;;; Devolve a utilidade de um dado tabuleiro para um jogador (a sua diferenca de pontos).
(defun utilidade (jogo id-jogador)
	(if (eq id-jogador 1)
		(- (jogo-pontos-jogador1 jogo) (jogo-pontos-jogador2 jogo))
		(- (jogo-pontos-jogador2 jogo) (jogo-pontos-jogador1 jogo))))

;;; historico-accoes: jogo --> lista
;;; Retorna uma lista com as jogadas feitas no jogo.
(defun historico-accoes (jogo)
	(jogo-historico-jogadas jogo))

;;; chave: jogo --> lista
;;; Cria uma chave para um estado, para ser usada como chave nas hastables usadas para
;;;  move-ordering e transposicao de nos.
(defun chave (jogo)
	(list (tabuleiro-fios (jogo-tabuleiro jogo))
		(jogo-jogador jogo)
		(jogo-pontos-jogador1 jogo)))

;;;;--------------------------------------------------------------
;;;;  Heuristicas

;;; Heuristica 1 - Moedas Isoladas
;;;  Analisa a diferenca de pontos entre os jogadores e os pontos que ainda estao em jogo.
;;;  Assume que consegue comer todas as moedas isoladas (moedas que so' tem um fio)
;;;   que ainda estao em jogo.
;;;  Devolve um inteiro equivalente 'a soma da diferenca dos pontos com o numero 
;;;   de pontos em jogo.
(defun H1 (jogo id-jogador)
	(let ((sum 0)
		  (posmoedas (moedasIsoladas jogo)))

		(when (not (eq posmoedas NIL))
			(dolist (pos posmoedas)
				(incf sum (tabuleiro-moeda-posicao (jogo-tabuleiro jogo) pos))
			)
		)

		(if (eq id-jogador 1)
			
			(+ (- (jogo-pontos-jogador1 jogo) (jogo-pontos-jogador2 jogo)) 
			   (tabuleiro-total-moedas (jogo-tabuleiro jogo))
			   sum)

			(+ (- (jogo-pontos-jogador2 jogo) (jogo-pontos-jogador1 jogo)) 
			   (tabuleiro-total-moedas (jogo-tabuleiro jogo))
			   sum)
		)
	)
)

;;; Heuristica 2 - Antevisao dos fios
;;;  Analisa a diferenca de pontos entre os jogadores e os pontos que 
;;;   ainda estao em jogo, somando aos seus pontos as linhas de fios que pode cortar.
;;;  Devolve um inteiro equivalente 'a soma da diferenca dos pontos com a soma 
;;;   das linhas.
(defun H2 (jogo id-jogador)
	(let ((linha (list NIL 0)))

		(setf linha (multiple-value-list (linhaMoedas jogo)))
		(if (= id-jogador 1)

			(if (= (jogo-jogador jogo) 1)
				(- (+ (jogo-pontos-jogador1 jogo) (second linha)) 
				   (jogo-pontos-jogador2 jogo))
				(- (jogo-pontos-jogador1 jogo)
				   (+ (jogo-pontos-jogador2 jogo) (second linha))) 
			)

			(if (= (jogo-jogador jogo) 2)
				(- (+ (jogo-pontos-jogador2 jogo) (second linha))
				   (jogo-pontos-jogador1 jogo))
				(- (jogo-pontos-jogador2 jogo)
				(+ (jogo-pontos-jogador1 jogo) (second linha)))
			)
		)
	)
)

;;; todasMoedas: jogo --> lista
;;; Retorna uma lista com todas as posicoes das moedas presentes num jogo.
(defun todasMoedas (jogo)
	(let ((lfios '())
		  (tabfios (tabuleiro-fios (jogo-tabuleiro jogo)))
		  (f-origem)
		  (f-destino))

		(dolist (f tabfios)
			(setf f-origem (fio-origem f))
			(setf f-destino (fio-destino f))
			(when (not (find f-origem lfios :test #'equal)) 
				(push f-origem lfios))
			(when (not (find f-destino lfios :test #'equal))
				(push f-destino lfios)))
		lfios))

;;; moedasIsoladas: jogo --> lista
;;; Retorna uma lista com as posicoes das moedas do jogo que so' se encontram ligadas
;;; por um fio.
(defun moedasIsoladas (jogo)
	(let*(  (tab (jogo-tabuleiro jogo))
			(lmoedas (todasMoedas jogo))
			(lfios   (tabuleiro-fios tab))
			(hits 0)
			(moedas '()))

		(dolist (moeda lmoedas)
			(dolist (fio lfios)
				(when (or (posicoes-iguais-p (fio-origem fio)  moeda) 
					      (posicoes-iguais-p (fio-destino fio) moeda))
					(incf hits 1)
					(when (> hits 1)
						(return -1)))
			)

			(when (= hits 1)
				(push moeda moedas))
			
			(setf hits 0)
		)
		moedas
	)
)

;;; moedaAdjacente: posicao x fio --> posicao
;;; Recebe a posicao de uma moeda, o fio a que esta ligada e retorna a outra moeda
;;; do fio.
(defun moedaAdjacente (posmoeda fio)
	(if (posicoes-iguais-p posmoeda (fio-origem fio))
		(fio-destino fio)
		(fio-origem  fio)))

;;; linhaMoedas: jogo --> int
;;; Recebe um jogo e retorna a soma de todas as moedas que pretencem a uma linha de fios.
;;; Percorre todas as moedas isoladas do jogo ate' que a linha acabe (que se ligue a uma 
;;; moeda ligada a varios fios ou que encontre outra moeda isolada) e calcula a soma do
;;; seu valor.
(defun linhaMoedas (jogo)
	(let ((fioAnterior) (moedasIso) (valorLinhas) 
		  (moedaAdj) (fiosMoeda) (total))
		
		;; qdo o tabuleiro nao e' uma linha
		(when (null (moedasIsoladas jogo))
			(return-from linhaMoedas (values NIL 0))
		)

		(setf moedasIso (moedasIsoladas jogo))
		(setf valorLinhas '())
		(setf total 0)
		
		;; Itera sobre as moedas isoladas. Para cada moeda isolada, tenta achar uma linha
		(dolist (m moedasIso)
			;; moeda inicial
			(incf total (tabuleiro-moeda-posicao (jogo-tabuleiro jogo) m))
			(setf fioAnterior (first (tabuleiro-fios-posicao (jogo-tabuleiro jogo) m)))
			(setf moedaAdj (moedaAdjacente m fioAnterior))
			(setf fiosMoeda (tabuleiro-fios-posicao (jogo-tabuleiro jogo) moedaAdj))
			;; percorre a linha da moeda
			(block contaLinha
			(loop
				;; moeda tem multiplos fios
				(when (> (length fiosMoeda) 2)
					(return-from contaLinha total))
				;; moeda
				(when (= (length fiosMoeda) 1)
					;; soma a moeda final
					(incf total (tabuleiro-moeda-posicao (jogo-tabuleiro jogo) moedaAdj))
					;; remove-a da lista 
					(if (= (length moedasIso) 1)
						(setf moedasIso '())
						(delete moedaAdj moedasIso))
					(return-from contaLinha total))
				
				;; soma a moeda adjacente ao total
				(incf total (tabuleiro-moeda-posicao (jogo-tabuleiro jogo) moedaAdj))
				(setf fiosMoeda (tabuleiro-fios-posicao (jogo-tabuleiro jogo) moedaAdj))
				;actualiza o fio anterior
				(if (equal (first fiosMoeda) fioAnterior)
					(setf fioAnterior (second fiosMoeda))
					(setf fioAnterior (first fiosMoeda)))
				
				;; calcula os novos fios para a nova moeda
				(setf moedaAdj (moedaAdjacente moedaAdj fioAnterior))
				(setf fiosMoeda (tabuleiro-fios-posicao (jogo-tabuleiro jogo) moedaAdj))
			))
		)
		(if (> total 0)
			(values T   total)
			(values NIL total))
	)
)


;;;;--------------------------------------------------------------
;;;;  MiniMax


;;; minimax: problema x id --> values (int x int x int)
;;; Recebe um problema e o id do jogador max inicial e executa um algoritmo minimax 
;;; simples (sem limite de tempo).
;;; Retorna o ID do fio escolhido, o valor da utilidade e o numero de nos folha expandidos.
(defun minimax (prob id)
	(let ((result (multiple-value-list (Value prob (Problema-estado-inicial prob) id #'>))))
		(values (first result) (second result) (third result))))

;;; minimax: problema x id --> values (int x int x int)
;;; Invoca o minimax normal, ignorando o limite de tempo recebido.
;;; Recebe um jogo, o ID do jogador max inicial e o tempo maximo de execucao e retorna
;;; o ID do fio escolhido, a utilidade e o numero de nos folha expandidos.
(defun jogador-minimax-simples (jogo id tempo)
	(declare (ignore tempo))
	(let ((problema (make-Problema
					   :estado-inicial 		jogo
				   	   :jogador   			'jogador
					   :accoes				'accoes
					   :resultado 			'resultado
					   :teste-corte-p 		'teste-terminal-p
					   :funcao-avaliacao 	'utilidade
					   :historico-accoes 	'historico-accoes)))
		(minimax problema id)))

;;; minimax-alfa-beta: problema x id --> values (int x int x int)
;;; Recebe um problema e o id do jogador max inicial e executa o minimax alfa-beta simples
;;; (sem limite de tempo e sem ordenacoes).
;;; Retorna o ID do fio escolhido, o valor da utilidade e o numero de nos folha expandidos.
(defun minimax-alfa-beta(prob id)
	(let ((result (multiple-value-list (Value prob (Problema-estado-inicial prob) id #'> -9999 9999))))
		(values (first result) (second result) (third result))))


;;; Value: problema x jogo x int x funcao x (opcional int x int x int) 
;;;  --> (values (int x int x int x int x int) )
;;;  Algoritmo MiniMax modificado de modo a que cada jogador possa jogar varias vezes,
;;; consoante a jogada efectuada, que fica ao encargo do jogo onde esta' a ser aplicado.
;;;  Executa um minimax simples ou um minimax alfa-beta, caso na chamada 'a funcao seja
;;; passado um alfa e um beta (-9999 e 9999, respectivamente, que representam
;;;  [-inf, +inf]).
;;; Retorna o id escolhido, a sua utilidade, o numero de nos folha visitados e os valores
;;; de alfa e beta (que sao desprezados na chamada 'a funcao).
(defun Value (prob estado idmax comparefunc
	&optional (alfa 0 alfa-supplied-p) (beta 0 beta-supplied-p) (depth NIL))
	(let ((best 0)
		  (chosen_id -1)
		  (accoes (funcall (Problema-accoes prob) estado))
		  (numfolhas  0)
		  (a)
		  (b))

		(if (eq comparefunc #'>)
			(setf best -9999)   ;Best para Max
			(setf best  9999))  ;Best para Min

		(when (and alfa-supplied-p beta-supplied-p)
			(setf a alfa) (setf b beta))

		;; No terminal --> devolve o id escolhido, a utilidade do tabuleiro e o valor do
		;;  no folha
		(when (funcall (Problema-teste-corte-p prob) estado depth)
			(return-from Value
				(values chosen_id
						(funcall (Problema-funcao-avaliacao prob) estado idmax) 
						1 a b)))

			;; Gera os sucessores do no e, consoante o proximo jogador a jogar,
		  	;; invoca o min ou o max para aquele sucessor.
			(dolist (id accoes)
				(let ((sucessor (funcall (Problema-resultado prob) estado id))
					  (lista_retorno))

					(if (and alfa-supplied-p beta-supplied-p)
						;; Minimax alfa-beta
						(if (eq (funcall (Problema-jogador prob) sucessor) idmax)
							(setf lista_retorno (multiple-value-list (Value prob sucessor idmax #'> a b depth)))
							(setf lista_retorno (multiple-value-list (Value prob sucessor idmax #'< a b depth))))
						;; Minimax normal
						(if (eq (funcall (Problema-jogador prob) sucessor) idmax)
							(setf lista_retorno (multiple-value-list (Value prob sucessor idmax #'>)))
							(setf lista_retorno (multiple-value-list (Value prob sucessor idmax #'<)))))

					;; Actualiza os valores retornados pelo no sucessor
					(when (funcall comparefunc (second lista_retorno) best)
						(setf best (second lista_retorno))
						(setf chosen_id id))
					(incf numfolhas (third lista_retorno))
					
					;; Usado apenas para os cortes alfa-beta
					(when (and alfa-supplied-p beta-supplied-p)
						(if (eq comparefunc #'>)
							(when (> (second lista_retorno) a)
								(setf a (second lista_retorno)))
							;else (#'<)
							(when (< (second lista_retorno) b)
								(setf b (second lista_retorno))))
						(when (>= a b) ;Corte
							(return-from Value (values chosen_id best numfolhas a b))))))
		(values chosen_id best numfolhas a b)))


;;; MINIMAX V1

;;; Minimax alfa-beta que trava a sua execucao consoante o tempo indicado, com 
;;;  uma margem de erro de 0.35 segundos para terminar a execucao. 
;;; Recebe um tempo-init que representa o tempo quando a chamada 'a funcao 
;;;  foi feita e um tempo que representa o tempo maximo em que o algoritmo pode executar.
;;; Reaproveita o ID escolhido como melhor no' da iteracao anterior de um fio,
;;; guardando-o numa hashtable (hashOrd), para acelerar a procura nas 
;;; iteracoes seguintes.
;;;  Recebe um problema para poder chamar as funcoes do tipo problema e uma funcao de
;;; comparacao que identifica se se trata de um no' min (#'<) ou max (#'>). A funcao e'
;;; invocada na actualizacao do no' (comparacao com o best), o que permite fazer
;;; a comparacao sem fazer condicoes diferentes.

;;; Valuevbest: problema x jogo x int x funcao x int x int
;;;      x int x int x int x hashtable --> values (int x int x int x int x int)
(defun Valuevbest (prob estado idmax comparefunc alfa beta
					depth tempo-init tempo)
	(let ((best 0)
		  (chosen_id -1)
		  (numfolhas  0)
		  (accoes (funcall (Problema-accoes prob) estado))
		  (a alfa)
		  (b beta)
		  )

		;;  No terminal --> devolve o id escolhido, a utilidade do tabuleiro 
		;; e o valor do no folha
		(when (funcall (Problema-teste-corte-p prob) estado depth)
			(return-from Valuevbest
				(values chosen_id
						(funcall (Problema-funcao-avaliacao prob) estado idmax)
						1
						a
						b
						(funcall (Problema-historico-accoes prob) estado))))
		
		(if (eq comparefunc #'>)
			(setf best -9999)   ;Best para Max
			(setf best  9999))  ;Best para Min
		
		(when (> (/ (- (get-internal-real-time) tempo-init) internal-time-units-per-second)
				(- tempo 0.35))
			(return-from Valuevbest (values -1 -1 -1 a b)))


			;; Gera os sucessores do no e, consoante o proximo jogador a jogar,
		  	;; invoca o min ou o max para aquele sucessor.
			(dolist (id accoes)
				(let ((sucessor (funcall (Problema-resultado prob) estado id))
					  (lista_retorno))
					(if (eq (funcall (Problema-jogador prob) sucessor) idmax)
						(setf lista_retorno (multiple-value-list 
							(Valuevbest prob sucessor idmax #'> a b 
								(+ depth 1) tempo-init tempo)))
						(setf lista_retorno (multiple-value-list 
							(Valuevbest prob sucessor idmax #'< a b 
								(+ depth 1) tempo-init tempo))))

					;; Actualiza os valores retornados pelo no sucessor,
					;;  consoante seja maior/menor.
					;; Caso o valor seja igual, prefere o no' com maior id
					(when (funcall comparefunc (second lista_retorno) best)
						(setf best (second lista_retorno))
						(setf chosen_id id))
					(incf numfolhas (third lista_retorno))
					;; Actualiza o alfa e o beta
					(if (eq comparefunc #'>) ;MaxValue
						(when (> (second lista_retorno) a)
							(setf a (second lista_retorno))) 
						;else (MinValue)
						(when (< (second lista_retorno) b)
							(setf b (second lista_retorno))))			
					;; Corte
					(when (>= a b)
						;; Guarda o ID do no' na hash de move-ordering
						(return-from Valuevbest (values chosen_id best numfolhas a b)))))

		(values chosen_id best numfolhas a b)
	)
)


;;; jogador-minimax-v0: jogo x int x int --> values (int x int x int)
;;;  Com limitacao do tempo e ordenacao dos nos (killer move heuristic).
;;; Corre a funcao Valuevbest-ord de forma iterativa, ate' atingir a profundidade maxima 
;;;  ou exceder o tempo recebido.
;;; Recebe um jogo, o ID do jogador max inicial e o tempo maximo de execucao e
;;;  retorna o melhor ID, o valor da utilidade e o numero de folhas visitadas.
(defun jogador-minimax-v0 (jogo id tempo)
	(let*  ((depth 		 1)
		    (tempo-init  (get-internal-real-time))
		    (result 	'())
		    (resultbk   '())
		    )
		(loop 
			(let* ((prob 	(make-Problema
								:estado-inicial  jogo
								:jogador   		 'jogador
								:accoes			 'accoes
								:resultado 		 'resultado
								:teste-corte-p   (lambda (jg curdepth) 
									(if (> curdepth depth) T (jogo-terminado-p jg)))
								:funcao-avaliacao 'H2
								:historico-accoes 'historico-accoes))
		    		(maxDepth  (length (funcall (Problema-accoes prob) (Problema-estado-inicial prob)))))

			(if (>= maxDepth depth)
				(progn  
					(setf resultbk (multiple-value-list
						(Valuevbest prob (Problema-estado-inicial prob) id #'> 
							-9999 9999 1 tempo-init tempo)))
					(if (> (/ (- (get-internal-real-time) tempo-init) internal-time-units-per-second) 
						   (- tempo 0.35))
						(progn 
							(return-from jogador-minimax-v0 (values (first result) (second result) (third result))))
						(setf result resultbk))
					(incf depth)
				)
				(progn 
					(return-from jogador-minimax-v0 (values (first result) (second result) (third result)))
				)
			))
		)
	)
)


;;; MINIMAX V1

;;; Minimax alfa-beta que trava a sua execucao consoante o tempo indicado, com 
;;;  uma margem de erro de 0.35 segundos para terminar a execucao. 
;;; Recebe um tempo-init que representa o tempo quando a chamada 'a funcao 
;;;  foi feita e um tempo que representa o tempo maximo em que o algoritmo pode executar.
;;; Reaproveita o ID escolhido como melhor no' da iteracao anterior de um fio,
;;; guardando-o numa hashtable (hashOrd), para acelerar a procura nas 
;;; iteracoes seguintes.
;;;  Recebe um problema para poder chamar as funcoes do tipo problema e uma funcao de
;;; comparacao que identifica se se trata de um no' min (#'<) ou max (#'>). A funcao e'
;;; invocada na actualizacao do no' (comparacao com o best), o que permite fazer
;;; a comparacao sem fazer condicoes diferentes.

;;; Valuevbest-ord: problema x jogo x int x funcao x int x int
;;;      x int x int x int x hashtable --> values (int x int x int x int x int)
(defun Valuevbest-ord (prob estado idmax comparefunc alfa beta
					depth tempo-init tempo hashOrd)
	(let ((best 0)
		  (chosen_id -1)
		  (numfolhas  0)
		  (accoes (funcall (Problema-accoes prob) estado))
		  (a alfa)
		  (b beta)
		  (melhorNo))

		;;  No terminal --> devolve o id escolhido, a utilidade do tabuleiro 
		;; e o valor do no folha
		(when (funcall (Problema-teste-corte-p prob) estado depth)
			(return-from Valuevbest-ord
				(values chosen_id
						(funcall (Problema-funcao-avaliacao prob) estado idmax)
						1
						a
						b
						(funcall (Problema-historico-accoes prob) estado))))
		
		(if (eq comparefunc #'>)
			(setf best -9999)   ;Best para Max
			(setf best  9999))  ;Best para Min
		
		(when (> (/ (- (get-internal-real-time) tempo-init) internal-time-units-per-second)
				(- tempo 0.35))
			(return-from Valuevbest-ord (values -1 -1 -1 a b)))

		;; Vai buscar o id do melhor no' filho e coloca-o em 1o na lista, caso o no' 
		;; ja' tenha sido expandido (ordenacao).
		(setf melhorNo (gethash (funcall (Problema-historico-accoes prob) estado) hashOrd))
		
		(when (and (not (null melhorNo))
				   (not (eq melhorNo -1)))
			;; coloca o melhor no' na cabeca da lista
			(if (eq (first accoes) melhorNo)
				(setf accoes (rest accoes))
				(delete melhorNo accoes))
			(push melhorNo accoes))

			;; Gera os sucessores do no e, consoante o proximo jogador a jogar,
		  	;; invoca o min ou o max para aquele sucessor.
			(dolist (id accoes)
				(let ((sucessor (funcall (Problema-resultado prob) estado id))
					  (lista_retorno))
					(if (eq (funcall (Problema-jogador prob) sucessor) idmax)
						(setf lista_retorno (multiple-value-list 
							(Valuevbest-ord prob sucessor idmax #'> a b 
								(+ depth 1) tempo-init tempo hashOrd)))
						(setf lista_retorno (multiple-value-list 
							(Valuevbest-ord prob sucessor idmax #'< a b 
								(+ depth 1) tempo-init tempo hashOrd))))

					;; Actualiza os valores retornados pelo no sucessor,
					;;  consoante seja maior/menor.
					;; Caso o valor seja igual, prefere o no' com maior id
					(when (funcall comparefunc (second lista_retorno) best)
						(setf best (second lista_retorno))
						(setf chosen_id id))
					(incf numfolhas (third lista_retorno))
					;; Actualiza o alfa e o beta
					(if (eq comparefunc #'>) ;MaxValue
						(when (> (second lista_retorno) a)
							(setf a (second lista_retorno))) 
						;else (MinValue)
						(when (< (second lista_retorno) b)
							(setf b (second lista_retorno))))			
					;; Corte
					(when (>= a b)
						;; Guarda o ID do no' na hash de move-ordering
						(setf (gethash (funcall (Problema-historico-accoes prob) estado) hashOrd) chosen_id)
						(return-from Valuevbest-ord (values chosen_id best numfolhas a b)))))

		(setf (gethash (funcall (Problema-historico-accoes prob) estado) hashOrd) chosen_id)
		(values chosen_id best numfolhas a b)
	)
)


;;; jogador-minimax-v1: jogo x int x int --> values (int x int x int)
;;;  Com limitacao do tempo e ordenacao dos nos (killer move heuristic).
;;; Corre a funcao Valuevbest-ord de forma iterativa, ate' atingir a profundidade maxima 
;;;  ou exceder o tempo recebido.
;;; Recebe um jogo, o ID do jogador max inicial e o tempo maximo de execucao e
;;;  retorna o melhor ID, o valor da utilidade e o numero de folhas visitadas.
(defun jogador-minimax-v1 (jogo id tempo)
	(let*  ((depth 		 1)
		    (tempo-init  (get-internal-real-time))
		    (result 	'())
		    (resultbk   '())
		    (hashOrd (make-hash-table :test #'equal)))
		(loop 
			(let* ((prob 	(make-Problema
								:estado-inicial  jogo
								:jogador   		 'jogador
								:accoes			 'accoes
								:resultado 		 'resultado
								:teste-corte-p   (lambda (jg curdepth) 
									(if (> curdepth depth) T (jogo-terminado-p jg)))
								:funcao-avaliacao 'H2
								:historico-accoes 'historico-accoes))
		    		(maxDepth  (length (funcall (Problema-accoes prob) (Problema-estado-inicial prob)))))

			(if (>= maxDepth depth)
				(progn  
					(setf resultbk (multiple-value-list
						(Valuevbest-ord prob (Problema-estado-inicial prob) id #'> 
							-9999 9999 1 tempo-init tempo hashOrd)))
					(if (> (/ (- (get-internal-real-time) tempo-init) internal-time-units-per-second) 
						   (- tempo 0.35))
						(progn 
							(return-from jogador-minimax-v1 (values (first result) (second result) (third result))))
						(setf result resultbk))
					(incf depth)
				)
				(progn 
					(clrhash hashOrd)
					(return-from jogador-minimax-v1 (values (first result) (second result) (third result)))
				)
			))
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jogador-minimax-v2 (jogo id tempo)
	(let*  ((depth 		 1)
		    (tempo-init  (get-internal-real-time))
		    (result 	'())
		    (resultbk   '())
		    (hashTransp (make-hash-table :test #'equal)))
		(loop
			(let* ((prob 	(make-Problema
								:estado-inicial   jogo
								:jogador   		  'jogador
								:accoes			  'accoes
								:resultado 		  'resultado
								:teste-corte-p    (lambda (jg curdepth)
									(if (> curdepth depth) T (jogo-terminado-p jg)))
								:funcao-avaliacao   'H2
								:historico-accoes   'historico-accoes
								:chave-equivalencia 'chave))
		    		(maxDepth  (length (funcall (Problema-accoes prob) (Problema-estado-inicial prob)))))

			(if (>= maxDepth depth)
				(progn
					(setf resultbk (multiple-value-list
						(Valuevbest-trans prob (Problema-estado-inicial prob) id #'> 
							-9999 9999 1 tempo-init tempo hashTransp)))
					(if (> (/ (- (get-internal-real-time) tempo-init) internal-time-units-per-second) 
							(- tempo 0.35))
						(progn
							(return-from jogador-minimax-v2 (values (first result) (second result) (third result))))
						(setf result resultbk))
					(incf depth)
				)
				(progn 
					(return-from jogador-minimax-v2 (values (first result) (second result) (third result)))
				)
			))
		(clrhash hashTransp)
		)
	)
)

(defun Valuevbest-trans (prob estado idmax comparefunc alfa beta depth
	tempo-init tempo hashTransp)
	(let ((best 0)
		  (chosen_id -1)
		  (numfolhas  0)
		  (accoes (funcall (Problema-accoes prob) estado))
		  (a alfa)
		  (b beta)
		  (minimaxTransp))

		;;  No terminal --> devolve o id escolhido, a utilidade do tabuleiro
		;; e o valor do no' folha
		(when (funcall (Problema-teste-corte-p prob) estado depth)
			(return-from Valuevbest-trans
				(values chosen_id
						(funcall (Problema-funcao-avaliacao prob) estado idmax)
						1
						a
						b
						(funcall (Problema-historico-accoes prob) estado))))
		
		(if (eq comparefunc #'>)
			(setf best -9999)   ;best para Max
			(setf best  9999))  ;best para Min
		
		(when (> (/ (- (get-internal-real-time) tempo-init) internal-time-units-per-second)
				(- tempo 0.35))
			(return-from Valuevbest-trans (values -1 -1 -1 a b)))

		(setf minimaxTransp (gethash (funcall (Problema-chave-equivalencia prob) estado) hashTransp))
		(when (not (null minimaxTransp)) 
			(return-from Valuevbest-trans
				(values chosen_id minimaxTransp 1 a b
					(funcall (Problema-historico-accoes prob) estado))))

			;; Gera os sucessores do no e, consoante o proximo jogador a jogar,
		  	;; invoca o min ou o max para aquele sucessor.
			(dolist (id accoes)
				(let ((sucessor (funcall (Problema-resultado prob) estado id))
					  (lista_retorno))
					(if (eq (funcall (Problema-jogador prob) sucessor) idmax)
						(setf lista_retorno (multiple-value-list 
							(Valuevbest-trans prob sucessor idmax #'> a b 
								(+ depth 1) tempo-init tempo hashTransp)))
						(setf lista_retorno (multiple-value-list 
							(Valuevbest-trans prob sucessor idmax #'< a b 
								(+ depth 1) tempo-init tempo hashTransp))))

					;; Actualiza os valores retornados pelo no' sucessor, 
					;;  consoante seja maior/menor. Caso o valor seja igual, 
					;;  prefere o no' com maior id.
					(when (funcall comparefunc (second lista_retorno) best)
						(setf best (second lista_retorno))
						(setf chosen_id id))
					(incf numfolhas (third lista_retorno))
					;; Actualiza o alfa e o beta
					(if (eq comparefunc #'>) ;MaxValue
						(when (> (second lista_retorno) a)
							(setf a (second lista_retorno))) 
						;else (MinValue)
						(when (< (second lista_retorno) b)
							(setf b (second lista_retorno))))			
					;; Corte
					(when (>= a b)
						(return-from Valuevbest-trans (values chosen_id best numfolhas a b)))))
		
		;; Guarda o valor minimax do no' na hash de transposicao
		(setf (gethash (funcall (Problema-chave-equivalencia prob) estado) hashTransp) best)
		(values chosen_id best numfolhas a b)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MINIMAX VBEST

;;;  Com limitacao de tempo, ordenacao dos nos (killer move heuristic)
;;;  e tabela de transposicao de nos.

;;; Funciona da mesma forma que a funcao Valuevbest-ord (alfa-beta com ordenacao
;;;  killer move), com a diferenca que utiliza uma tabela de transposicao 
;;;  que guarda o valor minimax de um no', de forma a nao expandir nos que 
;;;  representem a mesma situacao de jogo. Apos gerar os sucessores de um no',
;;;  guarda-o na tabela de transposicao de forma a retirar o seu valor minimax 
;;;  na proxima iteracao.

;;; Valuevbest-ord: problema x jogo x int x funcao x int x int
;;;      x int x int x int x hashtable x hashtable --> values (int x int x int)
;;; (As hashtables sao passadas por referencia)
;;; Retorna o ID escolhido, a sua utilidade e o numero de nos folha expandidos.
(defun Valuevbest-ord-trans (prob estado idmax comparefunc alfa beta depth
	tempo-init tempo hashOrd hashTransp)
	(let ((best 0)
		  (chosen_id -1)
		  (numfolhas  0)
		  (accoes (funcall (Problema-accoes prob) estado))
		  (a alfa)
		  (b beta)
		  (melhorNo)
		  (minimaxTransp))

		;;  No terminal --> devolve o id escolhido, a utilidade do tabuleiro
		;; e o valor do no' folha
		(when (funcall (Problema-teste-corte-p prob) estado depth)
			(return-from Valuevbest-ord-trans
				(values chosen_id
						(funcall (Problema-funcao-avaliacao prob) estado idmax)
						1
						a
						b
						(funcall (Problema-historico-accoes prob) estado))))
		
		(if (eq comparefunc #'>)
			(setf best -9999)   ;best para Max
			(setf best  9999))  ;best para Min
		
		(when (> (/ (- (get-internal-real-time) tempo-init) internal-time-units-per-second)
				(- tempo 0.35))
			(return-from Valuevbest-ord-trans (values -1 -1 -1 a b)))

		(setf minimaxTransp (gethash (funcall (Problema-chave-equivalencia prob) estado) hashTransp))
		(when (not (null minimaxTransp)) 
			(return-from Valuevbest-ord-trans
				(values chosen_id minimaxTransp 1 a b
					(funcall (Problema-historico-accoes prob) estado))))

		;; Vai buscar o id do melhor no' e coloca-o em 1o na lista, caso o no' tenha sido
		;; expandido (ordenacao)
		(setf melhorNo (gethash (funcall (Problema-historico-accoes prob) estado) hashOrd))
		
		(when (and (not (null melhorNo))
				   (not (eq melhorNo -1)))
			(if (eq (first accoes) melhorNo)
				(setf accoes (rest accoes))
				(delete melhorNo accoes))
			(push melhorNo accoes))

			;; Gera os sucessores do no e, consoante o proximo jogador a jogar,
		  	;; invoca o min ou o max para aquele sucessor.
			(dolist (id accoes)
				(let ((sucessor (funcall (Problema-resultado prob) estado id))
					  (lista_retorno))
					(if (eq (funcall (Problema-jogador prob) sucessor) idmax)
						(setf lista_retorno (multiple-value-list 
							(Valuevbest-ord-trans prob sucessor idmax #'> a b 
								(+ depth 1) tempo-init tempo hashOrd hashTransp)))
						(setf lista_retorno (multiple-value-list 
							(Valuevbest-ord-trans prob sucessor idmax #'< a b 
								(+ depth 1) tempo-init tempo hashOrd hashTransp))))

					;; Actualiza os valores retornados pelo no' sucessor, 
					;;  consoante seja maior/menor. Caso o valor seja igual, 
					;;  prefere o no' com maior id.
					(when (funcall comparefunc (second lista_retorno) best)
						(setf best (second lista_retorno))
						(setf chosen_id id))
					(incf numfolhas (third lista_retorno))
					;; Actualiza o alfa e o beta
					(if (eq comparefunc #'>) ;MaxValue
						(when (> (second lista_retorno) a)
							(setf a (second lista_retorno))) 
						;else (MinValue)
						(when (< (second lista_retorno) b)
							(setf b (second lista_retorno))))			
					;; Corte
					(when (>= a b)
						;; Guarda o ID do no' na hash de move-ordering
						(setf (gethash (funcall (Problema-historico-accoes prob) estado) hashOrd) chosen_id)
						(return-from Valuevbest-ord-trans (values chosen_id best numfolhas a b)))))
		
		;; Guarda o ID do no' na hash de move-ordering		
		(setf (gethash (funcall (Problema-historico-accoes prob) estado) hashOrd) chosen_id)
		;; Guarda o valor minimax do no' na hash de transposicao
		(setf (gethash (funcall (Problema-chave-equivalencia prob) estado) hashTransp) best)
		(values chosen_id best numfolhas a b)
	)
)

(defun jogador-minimax-vbest2 (jogo id tempo)
	(let*  ((depth 		 1)
		    (tempo-init  (get-internal-real-time))
		    (result 	'())
		    (resultbk   '())
		    (hashOrd    (make-hash-table :test #'equal))
		    (hashTransp (make-hash-table :test #'equal)))
		(loop
			(let* ((prob 	(make-Problema
								:estado-inicial   jogo
								:jogador   		  'jogador
								:accoes			  'accoes
								:resultado 		  'resultado
								:teste-corte-p    (lambda (jg curdepth)
									(if (> curdepth depth) T (jogo-terminado-p jg)))
								:funcao-avaliacao   'H1
								:historico-accoes   'historico-accoes
								:chave-equivalencia 'chave))
		    		(maxDepth  (length (funcall (Problema-accoes prob) (Problema-estado-inicial prob)))))

			(if (>= maxDepth depth)
				(progn
					(setf resultbk (multiple-value-list
						(Valuevbest-ord-trans prob (Problema-estado-inicial prob) id #'> 
							-9999 9999 1 tempo-init tempo hashOrd hashTransp)))
					(if (> (/ (- (get-internal-real-time) tempo-init) internal-time-units-per-second) 
							(- tempo 0.35))
						(progn
							(return-from jogador-minimax-vbest2 (values (first result) (second result) (third result))))
						(setf result resultbk))
					(incf depth)
				)
				(progn 
					(return-from jogador-minimax-vbest2 (values (first result) (second result) (third result)))
				)
			))
		(clrhash hashTransp)
		)
	)
)

;;; jogador-minimax-vbest: jogo x int x int --> values (int int int)
;;; Recebe um jogo, o ID do jogador max inicial e o tempo maximo de execucao e
;;; retorna o melhor ID, o valor da utilidade e o numero de folhas visitadas.

;;; Invoca o minimax com ordenacao killer move e tabela de transposicao
;;;  (Valuevbest-ord-trans) de forma iterativa, aumentando a profundidade de procura
;;;  a cada iteracao, ate atingir o valor maximo de tempo recebido (com uma margem
;;;  de erro de 0.35 segundos) ou atingir a profundidade maxima do jogo recebido.
(defun jogador-minimax-vbest (jogo id tempo)
	(let*  ((depth 		 1)
		    (tempo-init  (get-internal-real-time))
		    (result 	'())
		    (resultbk   '())
		    (hashOrd    (make-hash-table :test #'equal))
		    (hashTransp (make-hash-table :test #'equal)))
		(loop
			(let* ((prob 	(make-Problema
								:estado-inicial   jogo
								:jogador   		  'jogador
								:accoes			  'accoes
								:resultado 		  'resultado
								:teste-corte-p    (lambda (jg curdepth)
									(if (> curdepth depth) T (jogo-terminado-p jg)))
								:funcao-avaliacao   'H2
								:historico-accoes   'historico-accoes
								:chave-equivalencia 'chave))
		    		(maxDepth  (length (funcall (Problema-accoes prob) (Problema-estado-inicial prob)))))

			(if (>= maxDepth depth)
				(progn
					(setf resultbk (multiple-value-list
						(Valuevbest-ord-trans prob (Problema-estado-inicial prob) id #'> 
							-9999 9999 1 tempo-init tempo hashOrd hashTransp)))
					(if (> (/ (- (get-internal-real-time) tempo-init) internal-time-units-per-second) 
							(- tempo 0.35))
						(progn
							(return-from jogador-minimax-vbest (values (first result) (second result) (third result))))
						(setf result resultbk))
					(incf depth)
				)
				(progn 
					(return-from jogador-minimax-vbest (values (first result) (second result) (third result)))
				)
			))
		(clrhash hashTransp)
		)
	)
)

(load "interface-moedas.fas")
(load "exemplos.fas")