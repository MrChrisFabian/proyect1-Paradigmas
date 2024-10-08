#lang racket
(require racket/trace)

;;Funcion principal: ejecuta esto para empezar
;; (Probablemente no hace falta modificar esta funcion)
(define(inicio )
  (cond [(null? (imprimir(evaluar(validar-sintaxis(leer)))))null]
        [else inicio]))

;; funcion  encargada de validar la sintaxis de la expresion
;;
(define (validar-sintaxis expresion)
  ;; Define el patrón de la expresión regular
  (define patron "^\\(L [a-zA-Z][a-zA-Z0-9]* _ \\(.*\\)\\)$")
  (cond
    ;; Si la expresión es nula, retorna nulo
    [(null? expresion) null]
    ;; Convierte la expresión a una cadena y verifica si coincide con el patrón
    [(regexp-match? (regexp patron) (format "~a" expresion)) #t]
    ;; Si no coincide, retorna falso
    [else #f]))


;; funcion que dado un numero devuelve la formula de dado numero (solo positivos)
;; (ej: 1) por su formula (ej: (L f _ (L x _ (f x))) )
;;
;; Convierte un número en su representación como fórmula lambda
(define (numero-a-formula n)
  (cond
    [(= n 0) '(L f _ (L x _ x))]  ; Representación de 0
    [(> n 0) `(L f _ (L x _ ,(aplicar-fn n)))]))  ; Fórmula lambda para n positivo

;; Aplica 'f' n veces en una expresión lambda
(define (aplicar-fn n)
  (if (= n 1)
      'x
      `(f ,(aplicar-fn (- n 1)))))











;; convierte la formula a un numero
;; Ej:
;; (L f _ (L x _ (f x))) por el numero 1
;; o tambien
;; (L y _ (L p _ (y p))) por el numero 1
;;
;; (es la estructura que importa no el nombre de las variables!!)
;;



;; Función principal que convierte la fórmula lambda en un número de Church
(define (formula-a-numero formula)
  (cond
    [(null? formula) #f]  ; Si la fórmula es nula, retorna #f
    [(validar-sintaxis formula) (contador-aplicaciones (cadddr formula))]  ; Valida la sintaxis y cuenta las aplicaciones de la función
    [else ( print "Ingrese una fórmula válida")]))

;; Función auxiliar que cuenta cuántas veces se aplica la función
(define (contador-aplicaciones formula)
  (cond
    ;; Caso base: cuando llegamos a la variable (como 'x'), terminamos el conteo
    [(or (empty? formula) (symbol? formula)) 0]
    ;; Caso cuando tenemos una aplicación de `f` a algo
    [(and (list? formula) (symbol? (car formula)) (equal? (car formula) 'f))
     (+ 1 (contador-aplicaciones (cadr formula)))]  ;; Llamada recursiva al siguiente argumento
    ;; Caso cuando tenemos una lista de aplicaciones (por ejemplo, `(f (f x))`)
    [(list? (cadr formula))
     (contador-aplicaciones (cadr  formula))]  ;; Procesamos la lista anidada
    [else (list? (cdr formula))
     (contador-aplicaciones (cdr  formula))]))


;; recorre la expresion y sustituye todos los numeros por formulas
(define (sustituir-numeros expresion)
	(cond [(null? expresion) null]
	      ;; Modificar la siguiente expresion para que sustituya
	      ;; cada numero (ej: 1) por su formula (ej: (L f _ (L x _ f x)) )
	      ;; (probablemente te servira la funcion numero-a-formula)
              [else expresion]))

;; funcion para hacer reduccion beta
;; Ej: (L x _ (x x)) 2 ==> (2 2)
;; realiza 1 sola reduccion
(define (reduccion-beta expresion)
        (cond [(null? expresion) null]
              ;; Modificar para implementar reduccion beta de una expresion
              [else expresion]))


;; funcion encargada de evaluar la expresion y obtener un resultado
;; combina todas las funciones anteriores para sustituir, luego hacer reducciones
;; hasta obtener un resultado simplificado.. y finalmente sustituye formulas por
;; numeros.
;;
;; La funcion tambien imprime en cada paso lo que hizo, e indica la regla que
;; utilizo
;;
;; Ej:
;;     (((L x _ (L y _ (x y))) 1) 2)
(((L xy _ (xy))1)2)
1 2
fx ffx
;;
;; La funcion imprime (como efecto secundario) a la pantalla:
;;     (((L x _ (L y _ (x y))) (L f _ (L x _ (f x)))) 2)    [sust-num]
;;     (((L x _ (L y _ (x y))) (L f _ (L x _ (f x)))) (L f _ (L x _ (f (f x))))) [sust-num]
;;     ((L y _ ((L f _ (L x _ (f x))) y)))  (L f _ (L x _ (f (f x)))))    [red-beta]
;;     (L y _ ((L f _ (L x _ (f x))) (L f _ (L x _ (f (f x)))))))      [red-beta]
;;     (L y _ ((L f _ (L x _ (f x))) (L ff _ (L xx _ (ff (ff xx)))) )))   [sust-variables x -> xx y f --> ff]
;;     (L y _ (L x _ (L ff _ (L xx _ (ff (ff xx)) )) x))    [red-beta]
;;     (L y _ (L x _ (L xx _ (x (x xx)))))  [red-beta]
;;     (L y _ (L x _ (L xx _ (x (x xx)))))  [red-beta]
;;     (L y _  2)   [sust-formula  (L x _ (L xx _ (x (x xx))))--> 2]
;;
;; Pero devuelve:
;;     (L y _ 2)
;;
(define (evaluar expresion)
	(cond [(null? expresion) null]
              [else expresion]))

;;Funcion encargada de imprimir la expresion
(define (imprimir expresion)
	(cond [ (null? expresion) null]
		[else print expresion]))

;;funcion encargada de obtener datos del teclado
(define (leer )
	(read))

;;Funciones de depuracion
;;Habilite y deshabilite segun su necesidad
(trace inicio)
(trace validar-sintaxis)
;;(trace sustituir)
(trace evaluar)
(trace imprimir)
(trace leer)

