#lang racket
(require racket/trace)

;;Funcion principal: ejecuta esto para empezar
;; (Probablemente no hace falta modificar esta funcion)
(define (inicio)
  (cond
    [(null? (imprimir (evaluar (validar-sintaxis (leer))))) null?]
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
(define (numero-a-formula num)
  (cond
    [(not (number? num)) null?]
    ;; Modificar para que genere la formula para el
    ;; numero dado
    [else num]))

;; convierte la formula a un numero
;; Ej:
;; (L f _ (L x _ (f x))) por el numero 1
;; o tambien
;; (L y _ (L p _ (y p))) por el numero 1
;;
;; (es la estructura que importa no el nombre de las variables!!)
;;
(define (formula-a-numero formula)
  (cond
    [(null? formula) null?]
    [(validar-sintaxis formula) (contador (car (last (last formula))) (last (last formula)) )]
    [else (error "Ingrese una formula valida")]))
;; Contador que funciona como auxiliar
(define (contador ele lista)
(foldl + 0 (map (lambda (x) 1) (filter (lambda (x) (eq? ele x)) lista))))
;; recorre la expresion y sustituye todos los numeros por formulas
(define (sustituir-numeros expresion)
  (cond
    [(null? expresion) null?]
    ;; Modificar la siguiente expresion para que sustituya
    ;; cada numero (ej: 1) por su formula (ej: (L f _ (L x _ f x)) )
    ;; (probablemente te servira la funcion numero-a-formula)
    [else expresion]))

;; funcion para hacer reduccion beta
;; Ej: (L x _ (x x)) 2 ==> (2 2)
;; realiza 1 sola reduccion
(define (reduccion-beta expresion)
  (cond
    [(null? expresion) null?]
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
  (cond
    [(null? expresion) null?]
    [else expresion]))

;;Funcion encargada de imprimir la expresion
(define (imprimir expresion)
  (cond
    [(null? expresion) null?]
    [else
     print
     expresion]))

;;funcion encargada de obtener datos del teclado
(define (leer)
  (read))

;;Funciones de depuracion
;;Habilite y deshabilite segun su necesidad
(trace inicio)
;;(trace validar)
;;(trace sustituir)
(trace evaluar)
(trace imprimir)
(trace leer)
