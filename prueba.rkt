#lang racket
(require racket/trace)
(define alias-list '((SUMA . (L m _ (L n _ (L f _ (L x _ ((m f) ((n f) x)))))))))

(define (expandir-alias expresion)
  (cond
    [(symbol? expresion)
     (let ([alias (assoc expresion alias-list)]) (if alias (cdr alias) expresion))]
    [(list? expresion) (map expandir-alias expresion)]
    [else expresion]))

#| (define (numero-a-formula n)
     (if (= n 0)
         '(x)  ; 0 es la función que devuelve x
         `(L fx _ (f ,(numero-a-formula (- n 1)))))) ; n es L f _ (L x _ (f (n-1)))

   ;; Ejemplos de uso
   (display (numero-a-formula 0))  ;; => '(L f _ (L x _ x))
   (newline)
   (display (numero-a-formula 1))  ;; => '(L f _ (L x _ (f x)))
   (newline)
   (display (numero-a-formula 2))  ;; => '(L f _ (L x _ (f (f x))))
   (newline)'(L f _ (L x _ (f (f x))))
   (display (numero-a-formula 3))  ;; => '(L f _ (L x _ (f (f (f x)))))
   (newline)

 |#

;; Función principal que convierte la fórmula lambda en un número de Church
(define (formula-a-numero formula)
  (cond
    [(null? formula) #f] ; Si la fórmula es nula, retorna #f
    [(validar-sintaxis formula)
     (contador-aplicaciones
      (cadddr formula))] ; Valida la sintaxis y cuenta las aplicaciones de la función
    [else (print "Ingrese una fórmula válida")]))

;; Función auxiliar que cuenta cuántas veces se aplica la función
(define (contador-aplicaciones formula)
  (cond
    ;; Caso base: cuando llegamos a la variable (como 'x'), terminamos el conteo
    [(or (empty? formula) (symbol? formula)) 0]
    ;; Caso cuando tenemos una aplicación de `f` a algo
    [(and (list? formula) (symbol? (car formula)) (equal? (car formula) 'f))
     (+ 1 (contador-aplicaciones (cadr formula)))] ;; Llamada recursiva al siguiente argumento
    ;; Caso cuando tenemos una lista de aplicaciones (por ejemplo, `(f (f x))`)
    [(list? (cadr formula)) (contador-aplicaciones (cadr formula))] ;; Procesamos la lista anidada
    [else
     (list? (cdr formula))
     (contador-aplicaciones (cdr formula))]))

;; Valida la estructura básica de la fórmula lambda
(define (validar-sintaxis expresion)
  ;; Define el patrón de la expresión regular
  (define patron "^\\(L [a-zA-Z][a-zA-Z0-9]* _ (\\(.*\\)|[0-9]+|[a-zA-Z])\\)$")
  (cond
    ;; Si la expresión es nula, retorna nulo
    [(null? expresion) null]
    ;; Convierte la expresión a una cadena y verifica si coincide con el patrón
    [(regexp-match? (regexp patron) (format "~a" expresion)) #t]
    ;; Si no coincide, retorna falso
    [else #f]))

; ;; Ejemplos de uso
; (display (formula-a-numero '(L f _ (L x _ (f x))))) ;; Debería devolver 1
; (newline)
; (display (formula-a-numero '(L f _ (L x _ (f (f x)))))) ;; Debería devolver 2
; (newline)
; (display (formula-a-numero '(L f _ (L x _ (f (f (f x))))))) ;; Debería devolver 3
; (newline)
; (display (formula-a-numero '(L f _ (L x _ (f (f x)))))) ;; Debería devolver 2
; (newline)

;; Convierte un número en su representación como fórmula lambda
(define (numero-a-formula n)
  (cond
    [(= n 0) '(L f _ (L x _ x))] ; Representación de 0
    [(> n 0) `(L f _ (L x _ ,(aplicar-fn n)))])) ; Fórmula lambda para n positivo

;; Aplica 'f' n veces en una expresión lambda
(define (aplicar-fn n)
  (if (= n 0) 'x `(f ,(aplicar-fn (- n 1)))))

;; Recorre la expresión y sustituye números por fórmulas lambda
(define (sustituir-numeros expresion)
  (cond
    [(number? expresion) (numero-a-formula expresion)] ;; Si es un número, lo sustituimos
    [(list? expresion)
     (map sustituir-numeros expresion)] ;; Si es una lista, aplicamos la función recursivamente
    [(null? expresion) null] ;; Si la expresión está vacía, retornamos null
    [else expresion])) ;; Cualquier otra cosa queda igual

;; Ejemplos
; (display (sustituir-numeros '(+ 1 (* 2 3))))  ;; Sustituirá los números por fórmulas lambda
(newline)
; (display (sustituir-numeros '(f (L x _ (+ x 2)))))  ;; Dejará intactas las partes no numéricas
(newline)

;; Función para reemplazar todas las ocurrencias de `var` en `body` con `arg`
(define (sustituir var arg body)
  (cond
    [(symbol? body)
     (if (equal? body var) arg body)] ;; Si es un símbolo y es igual a var, lo sustituimos
    [(list? body)
     (map (lambda (x) (sustituir var arg x)) body)] ;; Si es una lista, aplicamos recursivamente
    [else body])) ;; Si es otra cosa, la dejamos igual

;; Función para realizar una sola reducción beta sin usar let
(define (reduccion-beta expresion)
  (cond
    ;; Si la expresión es una aplicación de la forma ((L var _ body) arg)
    [(and (list? expresion) (list? (car expresion)) (equal? (caar expresion) 'L))
     ;; Obtenemos la variable y el cuerpo de la lambda, sustituimos el argumento
     (sustituir (cadar expresion) (cadr expresion) (cadddr (car expresion)))]
    ;; Caso recursivo: si es una lista, procesamos cada elemento
    [(list? expresion) (map reduccion-beta expresion)]
    [else expresion])) ;; Si no es una lista, devolvemos la expresión tal cual
;; Ejemplo

; (display (reduccion-beta '((L x _ (x x)) 2))) ;; Debería retornar (2 2)
(newline)
; (display (reduccion-beta '((L x _ (L y _ (x y))) 3)))  ;; Debería retornar (L y _ (3 y))
(newline)

;; Función para evaluar una expresión lambda
(define (evaluar expresion)
  (define (evaluar-aux expr paso)
    (cond
      ;; Si la expresión es nula, devolvemos null
      [(null? expr) null]
      ;; Si la expresión es un número, realizamos la sustitución y mostramos el paso
      [(number? expr)
       (printf "~a    [sust-num]\n" (sustituir-numeros expr))
       (evaluar-aux (sustituir-numeros expr) (add1 paso))]
      ;; Aplicar reducción beta si es necesario y mostrar el paso
      [(and (list? expr) (list? (car expr)) (equal? (caar expr) 'L))
       (printf "~a    [red-beta]\n" (reduccion-beta expr))
       (evaluar-aux (reduccion-beta expr) (add1 paso))]
      ;; Continuar evaluando cualquier lista de expresiones
      [(list? expr)
       (map (lambda (x) (evaluar-aux x paso)) expr)]
      ;; Devolver la expresión si es irreductible
      [else expr]))

  ;; Iniciar la evaluación desde el paso 1
  (evaluar-aux expresion 1))

; (display (evaluar '(((L x _ (L y _ (x y))) 1) 2)))
(newline)
; (display (evaluar '((SUMA 1) 2)))
(newline)
; (evaluar '(((L m _ ( L n _ f x n f m f x)) 1) 2))
; '(((L m  ( L n  f x n f m f x)) 1) 2)
; '(((L mnfx _ nf(mfx))1)2)
; (evaluar '(((L mnfx _ nf(mfx))1)2))
; '(L m _ (L n _ (L f _ (L x _ n f( m f x)))))

(reduccion-beta '((L m _ (L n _ (L f _ (L x _ n f ( m f x)))))1))
; resultado de (L n _ (L f _ (L x _ n f (1 f x))))
(reduccion-beta '((L n _ (L f _ (L x _ n f (1 f x))))2))
;resultado de '(L f _ (L x _ 2 f (1 f x)))
(sustituir-numeros '(L f _ (L x _ 2 f (1 f x))))
; resultado de '(L f _ (L x _ (L f _ (L x _ (f (f x)))) f ((L f _ (L x _ (f x))) f x)))
(evaluar '(L f _ (L x _ (L f _ (L x _ (f (f x)))) f (((L f _ (L x _ (f x))) f ) x ))))
;resultado '(L f _ (L x _ (L f _ (L x _ (f (f x)))) f ((L x _ (f x)) x)))
(reduccion-beta '(L f _ (L x _ (L f _ (L x _ (f (f x)))) f ((L x _ (f x)) x))))
; resultado '(L f _ (L x _ (L f _ (L x _ (f (f x)))) f (f x)))
(reduccion-beta '(L f _ (L x _ ((L f _ (L x _ (f (f x)))) f )(f x))))
; resultado '(L f _ (L x _ (L x _ (f (f x))) (f x)))
(reduccion-beta '(L f _ (L x _ ((L x _ (f (f x))) (f x)))))
; resultado '(L f _ (L x _ (f (f (f x))))) Wiii

; (evaluar '(((L m _ (L n _ (L f _ (L x _ n f( m f x)))))1)2))
; (evaluar '(((L xy _ (xy))1)2))
(trace evaluar)
(trace reduccion-beta)
(define (ultimo lista)
  (cond
    [(null? (cdr lista)) (car lista)]
    [else (ultimo (cdr lista))]))
