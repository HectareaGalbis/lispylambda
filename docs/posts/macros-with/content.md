<a id="TITLE:ADP-GITHUB:TAG219"></a>
# Las macros with

* [Introducción](/docs/posts/macros-with/content.md#TITLE:ADP-GITHUB:TAG220)
* [Definiendo una macro with](/docs/posts/macros-with/content.md#TITLE:ADP-GITHUB:TAG221)
* [El operador ```unwind-protect```](/docs/posts/macros-with/content.md#TITLE:ADP-GITHUB:TAG222)
* [Una variable auxiliar](/docs/posts/macros-with/content.md#TITLE:ADP-GITHUB:TAG223)
* [Un detalle sutil](/docs/posts/macros-with/content.md#TITLE:ADP-GITHUB:TAG224)
* [Añadiendo declaraciones](/docs/posts/macros-with/content.md#TITLE:ADP-GITHUB:TAG225)
* [Conclusión](/docs/posts/macros-with/content.md#TITLE:ADP-GITHUB:TAG226)


<a id="TITLE:ADP-GITHUB:TAG220"></a>
## Introducción

La familia de macros ```with``` es sin duda una de las más conocidas en Common Lisp\. Y es que a pesar de que existe el _Garbage Collector_ existen objetos que requieren de una inicialización y una terminación\.

El ejemplo más claro es el uso de ficheros\. Para poder leer o escribir sobre un fichero hay que abrirlo\. Y cuando ya hemos terminado\, debemos cerrarlo\.

`````common-lisp
(let ((file (open "~/file.txt")))
  ;; Leemos o escribimos en el fichero
  (close file)) ; Cerramos el fichero
`````

Pero esto tiene un problema\. Se nos puede olvidar cerrarlo\. Parece una tontería\, pero pasa mucho\.

Además\, también podemos ver que hay redundancia de información\. Estamos usando [let](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm) para crear una variable local ```file``` con el objeto que representa al archivo abierto\. Lo más común es que sólo lo usemos dentro de este [let](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm)\, y si no hacemos ninguna asignación ni siquiera será posible usar este fichero desde fuera al no tener acceso a la variable ```file```\. Así que\, ¿no debería ser obvio que el fichero tiene que cerrarse al acabar el [let](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm)\?

La expresión [let](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm) no es capaz de averiguar esto por sí sólo\. ¡Y tampoco debe\! Aunque lo normal es usar el fichero sólo dentro del [let](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm) aún podemos asignar el fichero a una variable externa y cerrar el fichero más tarde\.

El concepto de que algo vive de forma limitada en Common Lisp se conoce como **dynamic extent**\. En el ejemplo anterior hemos creado un fichero con _dynamic extent_\. El fichero vive de manera limitada durante la ejecución de la expresión [let](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm)\. Sin embargo\, el hecho de que tenga _dynamic extent_ es expresado de manera explícita mediante el uso de la función [close](http://www.lispworks.com/reference/HyperSpec/Body/f_close.htm)\. Y es claro que un archivo abierto con [open](http://www.lispworks.com/reference/HyperSpec/Body/f_open.htm) debe ser cerrado con [close](http://www.lispworks.com/reference/HyperSpec/Body/f_close.htm)\, por lo que el uso de esta función es ciertamente redundante\.

Para estos casos se usan las macros ```with```\. Una macro ```with```\, en general\, va a definir un objeto con _dynamic extent_\. Es decir\, va a definir un objeto que estará disponible durante un tiempo limitado\. De hecho\, una buena macro ```with``` debe asegurarse de que el objeto en cuestión es finalizado sí o sí\, independientemente de si la ejecución ha sido correcta o ha ocurrido algún error\. Dicho de otra forma\, si salimos de la expresión ```with```\, el objeto que haya definido debe finalizarse sin falta\.


<a id="TITLE:ADP-GITHUB:TAG221"></a>
## Definiendo una macro with

Siguiendo el ejemplo del fichero\, vamos a crear una macro que llamaremos ```with-file``` que abra un fichero y lo cierre automáticamente al terminarse la macro\.

La macro debe saber cómo abrir el fichero a qué variable debe asignarle el objeto que representa el fichero\. Así que al menos debe recibir un símbolo para la variable y los argumentos que le pasaremos luego a la función [open](http://www.lispworks.com/reference/HyperSpec/Body/f_open.htm)\. No nos olvidemos de las expresiones que se tienen que ejecutar teniendo el fichero abierto\.

Una primera versión de nuestra macro ```with-file``` podría ser esta\:

`````common-lisp
(defmacro with-file (var (&rest args) &body body)
  `(let ((,var (open ,@args)))
     ,@body
     (close ,var)))
`````
`````common-lisp
;; Returns
WITH-FILE
`````

La macro devuelve exactamente el código del ejemplo\. Se crea un fichero\, lo usamos en el cuerpo de la macro\, y finalmente se cierra\. Suena bien\.

`````common-lisp
(with-file mi-fichero ("~/file.txt" :direction :output :if-does-not-exist :create :if-exists :supersede)
  (prin1 "Hola mundo" mi-fichero))

(with-file mi-fichero ("~/file.txt")
  (read mi-fichero))
`````
`````common-lisp
;; Returns
T
`````

¡Genial\! Parece que\.\.\. Espera un momento\.\.\. Primero he escrito ```"Hola mundo"``` en el fichero ```~/file.txt``` \(lo he comprobado en mi pc\)\. Y luego he devuelto lo que devolvía ```(read mi-fichero)``` que debería ser ```"Hola mundo"``` de nuevo\. Sin embargo\, se ha devuelto el valor ```T```\.

Vamos a debuguear\. Vamos a expandir la macro cuando estamos leyendo a ver si encontramos algo raro\.

`````common-lisp
(macroexpand-1 '(with-file mi-fichero ("~/file.txt")
                  (read mi-fichero)))
`````
`````common-lisp
;; Returns
(LET ((MI-FICHERO (OPEN "~/file.txt")))
  (READ MI-FICHERO)
  (CLOSE MI-FICHERO))
T
`````

¡Ajá\! Por un momento he pensado que se devolvía ```(read mi-fichero)``` pero la última expresión es la llamada a la función [close](http://www.lispworks.com/reference/HyperSpec/Body/f_close.htm)\. Y esta función devuelve ```T``` si el stream recibido \(en este caso el fichero\) estaba abierto\. Todo cuadra\.

Pues toca arreglar nuestra macro para que pueda devolver la última expresión\. Podría ser algo así\:

`````common-lisp
(defmacro with-file (var (&rest args) &body body)
  (alexandria:with-gensyms (result-sym)
    `(let* ((,var (open ,@args))
            (,result-sym (progn ,@body)))
       (close ,var)
       ,result-sym)))
`````
`````common-lisp
;; Returns
WITH-FILE
`````

Aquí estoy usando ```alexandria:with-gensyms``` para crear un símbolo no internado para que nuestra macro sea [higiénica](https://en.wikipedia.org/wiki/Hygienic_macro)\. Por otro lado\, observa que se está usando [let\*](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm) en lugar de [let](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm)\. Esto es para que las expresiones en ```body``` tengan acceso a la variable ```,var```\.

Probemos de nuevo a leer el fichero ```"~/file.txt"```

`````common-lisp
(with-file mi-fichero ("~/file.txt")
  (read mi-fichero))
`````
`````common-lisp
;; Returns
"Hola mundo"
`````

¡Ahora sí\! Aunque por si acaso\, vamos a comprobar que realmente se cierra el fichero\. Vamos a crear una variable fuera de la expresión ```with-file``` y le asignaremos el fichero que se ha abierto\. Una vez finalizada la expresión ```with-file``` la función [open\-stream\-p](http://www.lispworks.com/reference/HyperSpec/Body/f_open_s.htm) debe devolver ```NIL```\.

`````common-lisp
(let (mi-fichero-externo)
  (with-file mi-fichero ("~/file.txt")
    (format t "Dentro de WITH-FILE: ~a~%" (open-stream-p mi-fichero))
    (setf mi-fichero-externo mi-fichero))
  (format t "Fuera de WITH-FILE: ~a" (open-stream-p mi-fichero-externo)))
`````
`````text
;; Output
Dentro de WITH-FILE: T
Fuera de WITH-FILE: NIL
`````


¡Perfecto\!

Pues no se tú\, pero yo creo que la macro está ya perfecta\, ¿verdad\.\.\.\?

`````common-lisp
(let (mi-fichero-externo)
  (ignore-errors (with-file mi-fichero ("~/file.txt")
                   (setf mi-fichero-externo mi-fichero)
                   (error "Oh no!!")))
  (format t "Fuera de WITH-FILE: ~a" (open-stream-p mi-fichero-externo))) ; ¡¡¡Sigue abierto!!!
`````
`````text
;; Output
Fuera de WITH-FILE: T
`````


¡Oh no\! ¡En este ejemplo al lanzarse una excepción el fichero no se ha cerrado\!


<a id="TITLE:ADP-GITHUB:TAG222"></a>
## El operador ```unwind-protect```

Tras el error anterior es tentador pensar en alguna solución que involucre capturar la excepción y relanzarla tras haber cerrado el fichero\. Pero hay otras expresiones que pueden sacarnos de la macro ```with-file``` sin haber cerrado el fichero\, como por ejemplo\, [return\-from](http://www.lispworks.com/reference/HyperSpec/Body/s_ret_fr.htm)\.

Pensar en todos los casos posibles donde podemos salir de ```with-file``` de manera no controlada para tratar de tomar el control es simplemente inviable\.

En lugar de eso\, podemos usar un operador que ya existe en el estándar de Common Lisp y que hace justamente lo que queremos\. Hablamos de [unwind\-protect](http://www.lispworks.com/reference/HyperSpec/Body/s_unwind.htm)\.

Según el estándar [unwind\-protect](http://www.lispworks.com/reference/HyperSpec/Body/s_unwind.htm) hace lo siguiente\:

> **unwind\-protect** _protected\-form cleanup\-form\* \=\> result\*_<br>
> <br>
> **unwind\-protect** evaluates _protected\-form_ and guarantees that _cleanup\-forms_ are executed before **unwind\-protect** exits\, whether it terminates normally or is aborted by a control transfer of some kind\.

Es decir\, evalúa la expresión que le pasemos como primer argumento y se asegura de que el resto de argumentos se evalúen tras evaluar el primer argumento\. Podemos ver algún ejemplo\:

`````common-lisp
(unwind-protect
    (print "Hola")
  (print "Adios"))
`````
`````text
;; Output

"Hola" 
"Adios" 
`````
`````common-lisp
;; Returns
"Hola"
`````

Por ahora nada especial\. Primero se evalúa ```(print "Hola")``` y luego ```(print "Adios")```\. Aunque sí cabe destacar que el valor devuelto por [unwind\-protect](http://www.lispworks.com/reference/HyperSpec/Body/s_unwind.htm) es el valor del primer argumento\. En este caso se devuelve el valor de ```(print "Hola")``` que es precisamente ```"Hola"```\.

Probemos a usar una excepción\:

`````common-lisp
(ignore-errors
  (unwind-protect
      (progn (print "Hola")
             (error "Oh no!"))
    (print "Adios")))
`````
`````text
;; Output

"Hola" 
"Adios" 
`````


¡Genial\! También funciona\.

De hecho\, si quitamos [unwind\-protect](http://www.lispworks.com/reference/HyperSpec/Body/s_unwind.htm) veremos que ```"Adios"``` no se imprime\:

`````common-lisp
(ignore-errors
  (print "Hola")
  (error "Oh no!")
  (print "Adios"))
`````
`````text
;; Output

"Hola" 
`````


Pues ya toca usar [unwind\-protect](http://www.lispworks.com/reference/HyperSpec/Body/s_unwind.htm) en nuestra macro ```with-file```\. La macro quedaría así\:

`````common-lisp
(defmacro with-file (var (&rest args) &body body)
  `(let* ((,var (open ,@args)))
     (unwind-protect
         (progn ,@body)
       (close ,var))))
`````
`````common-lisp
;; Returns
WITH-FILE
`````

Recuerda que en la anterior versión de la macro ```with-file``` guardábamos el resultado en una variable auxiliar\. Ahora ya no hace falta pues [unwind\-protect](http://www.lispworks.com/reference/HyperSpec/Body/s_unwind.htm) devolverá lo que devuelva su primer argumento\, que en nuestro caso es todo el cuerpo\. El segundo argumento de [unwind\-protect](http://www.lispworks.com/reference/HyperSpec/Body/s_unwind.htm) es ```(close ,var)```\, de manera que siempre se cerrará el fichero cuando salgamos abruptamente de la macro ```with-file```\.

`````common-lisp
(let (mi-fichero-externo)
  (ignore-errors (with-file mi-fichero ("~/file.txt")
                   (setf mi-fichero-externo mi-fichero)
                   (error "Oh no!!")))
  (format t "Fuera de WITH-FILE: ~a" (open-stream-p mi-fichero-externo))) ; ¡Ahora sí se cierra!
`````
`````text
;; Output
Fuera de WITH-FILE: NIL
`````


Ahora sí\, ¿verdad\? Yo creo que nuestra macro ```with-file``` ya está perfectísima\. Bueno\.\.\.

`````common-lisp
(with-file mi-fichero ("~/file.txt")
  (setf mi-fichero 3))
`````

`````common-lisp
The value
  3
is not of type
  STREAM
   [Condition of type TYPE-ERROR]
`````


<a id="TITLE:ADP-GITHUB:TAG223"></a>
## Una variable auxiliar

Quizás estés pensando que lo anterior es algo forzado\. Estamos asignando un valor a ```mi-fichero``` para que al usarse [close](http://www.lispworks.com/reference/HyperSpec/Body/f_close.htm) se produzca un error\. Pero hay que recordar que la premisa de una macro ```with``` es que el objeto dure tanto como la propia expresión\. Y al producirse el error en [close](http://www.lispworks.com/reference/HyperSpec/Body/f_close.htm) estamos saliendo de la macro \(de manera abrupta\) sin que el fichero se haya cerrado\. Por tanto\, la macro sigue sin estar perfecta\.

El problema está en que estamos usando la variable elegida por quien use la macro ```with-file``` para cerrar el fichero\. Y quien use la macro ```with-file``` puede hacer lo que quiera\, incluido asignar un nuevo valor a la variable\.

Así que la solución es sencilla\. Usemos una variable diferente que no se pueda usar desde fuera\. Es decir\, un símbolo no internado\. Cuando abramos el fichero asignaremos el objeto a dos variables diferentes\. Una la pasada como argumento y otra interna creada por la propia macro\.

La macro quedaría así\:

`````common-lisp
(defmacro with-file (var (&rest args) &body body)
  (alexandria:with-gensyms (aux-sym)
    `(let* ((,var (open ,@args))
            (,aux-sym ,var))
       (unwind-protect
           (progn ,@body)
         (close ,aux-sym)))))
`````
`````common-lisp
;; Returns
WITH-FILE
`````

¡Listo\! No ha sido tan difícil\. Probemos de nuevo el ejemplo\:

`````common-lisp
(with-file mi-fichero ("~/file.txt")
  (setf mi-fichero 3))
`````
`````common-lisp
;; Returns
3
`````

¡Genial\! Ya no da error\. Pero asegurémonos de que se cierra el fichero\:

`````common-lisp
(let (mi-fichero-externo)
  (with-file mi-fichero ("~/file.txt")
    (setf mi-fichero-externo mi-fichero)
    (setf mi-fichero 3))
  (format t "Fuera de WITH-FILE: ~a" (open-stream-p mi-fichero-externo)))
`````
`````text
;; Output
Fuera de WITH-FILE: NIL
`````


¡Perfecto\! Aunque la macro aún no\.\.\.


`````common-lisp
(with-file mi-fichero ("~/file.txt")
  nil)
`````
`````common-lisp
;; Returns
NIL
`````

¿Ves el problema\? ¿No\? Pues ahí está precisamente lo malo\.


<a id="TITLE:ADP-GITHUB:TAG224"></a>
## Un detalle sutil

Quizás lo siguiente pueda parecer innecesario\, pero a mi me gusta que todo esté lo más perfecto posible\. También es cierto que toda esta sección depende de la implementación que estés usando\, pues los mensajes de warning o errores pueden variar de una a otra\. En mi caso estoy usando SBCL\.

Volvamos a ver el ejemplo anterior\:

`````common-lisp
(with-file mi-fichero ("~/file.txt")
  nil)
`````
`````common-lisp
;; Returns
NIL
`````

Parece normal\, pero observa ahora qué ocurre con una expresión [let](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm)\:

`````common-lisp
(let (mi-fichero)
  nil)
`````
`````common-lisp
;; Returns
NIL
`````

`````common-lisp
;; Warning
in: PROGN (LET (MI-FICHERO)
         NIL)
    (LET (ADP-GITHUB::MI-FICHERO)
      NIL)

caught STYLE-WARNING:
  The variable MI-FICHERO is defined but never used.
`````

¡Ajá\! Ahora sí se ve el problema\. La expresión [let](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm) nos avisa de que no estamos usando la variable ```mi-fichero```\. Sin embargo\, nuestra macro ```with-file``` no lo ha hecho\.

Veamos el porqué expandiendo el ejemplo\:

`````common-lisp
(macroexpand-1 '(with-file mi-fichero ("~/file.txt")
                  nil))
`````
`````common-lisp
;; Returns
(LET* ((MI-FICHERO (OPEN "~/file.txt")) (#:AUX-SYM652 MI-FICHERO))
  (UNWIND-PROTECT (PROGN NIL) (CLOSE #:AUX-SYM652)))
T
`````

Observa que ```mi-fichero``` se define con la función [open](http://www.lispworks.com/reference/HyperSpec/Body/f_open.htm)\. Y luego se define la variable ```#:AUX-SYM```\.\.\. ¡usando la variable ```mi-fichero```\! Por eso no estamos recibiendo el warning\, porque siempre se está usando\.

Pero el arreglo es sencillo\, podemos definir las variables en el orden contrario\.

`````common-lisp
(defmacro with-file (var (&rest args) &body body)
  (alexandria:with-gensyms (aux-sym)
    `(let* ((,aux-sym (open ,@args)) ; <- Primero aux-sym
            (,var ,aux-sym))         ; <- Segundo var
       (unwind-protect
           (progn ,@body)
         (close ,aux-sym)))))
`````
`````common-lisp
;; Returns
WITH-FILE
`````

Ahora sí deberíamos recibir el warning\:

`````common-lisp
(with-file mi-fichero ("~/file.txt")
  nil)
`````
`````common-lisp
;; Returns
NIL
`````

`````common-lisp
;; Warning
in: PROGN (WITH-FILE MI-FICHERO
           ("~/file.txt")
         NIL)
    (ADP-GITHUB::MI-FICHERO #:AUX-SYM624)

caught STYLE-WARNING:
  The variable MI-FICHERO is defined but never used.
`````

¡Ahora sí\! Y ahora que sí nos avisa\, si realmente queremos que no nos lance el warning podemos usar una declaración como ```(declare (ignore mi-fichero))```\:

`````common-lisp
(with-file mi-fichero ("~/file.txt")
  (declare (ignore mi-fichero))
  nil)
`````

`````common-lisp
;; Error
caught ERROR:
  There is no function named DECLARE.
`````

¡Pero será hijo de \.\.\.\!


<a id="TITLE:ADP-GITHUB:TAG225"></a>
## Añadiendo declaraciones

Bueno\, al igual que en los anteriores casos\, vamos a expandir el ejemplo para ver mejor porqué obtenemos un error al usar una declaración\:

`````common-lisp
(macroexpand-1 '(with-file mi-fichero ("~/file.txt")
                  (declare (ignore mi-fichero))
                  nil))
`````
`````common-lisp
;; Returns
(LET* ((#:AUX-SYM654 (OPEN "~/file.txt")) (MI-FICHERO #:AUX-SYM654))
  (UNWIND-PROTECT (PROGN (DECLARE (IGNORE MI-FICHERO)) NIL)
    (CLOSE #:AUX-SYM654)))
T
`````

Fíjate que la declaración está justo dentro de la expresión [progn](http://www.lispworks.com/reference/HyperSpec/Body/s_progn.htm)\. Y claro\, esta expresión no acepta declaraciones\. Es más\, las declaraciones que hagamos van estar relacionadas con el símbolo ```mi-fichero```\. Es decir\, las declaraciones deberían situarse justo al empezar el cuerpo de la expresión [let](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm)\.

`````common-lisp
(defmacro with-file (var (&rest args) &body body)
  (alexandria:with-gensyms (aux-sym)
    `(let* ((,aux-sym (open ,@args))
            (,var ,aux-sym))
                                ; <-- Aquí deberían ir las declaraciones
       (unwind-protect
           (progn ,@body)
         (close ,aux-sym)))))
`````

Normalmente las declaraciones estarán en el propio cuerpo de la macro ```with-file```\, es decir\, la variable ```body```\. Una opción podría ser recibir un nuevo argumento justo antes de ```body```\:

`````common-lisp
(defmacro with-file (var (&rest args) declaration &body body)
  ...
  )
`````

Pero las declaraciones son opcionales\. Y además\, todas las expresiones que aceptan declaraciones suelen permitir varias líneas de declaraciones\. Es decir\, que lo siguiente debería ser válido\:

`````common-lisp
(with-file mi-fichero ("~/file.txt")
  (declare (special mi-fichero))
  (declare (ignorable mi-fichero))
  ...
  )
`````

No hay más remedio que extraer las declaraciones de la variable ```body```\. Basta con realizar un [loop](http://www.lispworks.com/reference/HyperSpec/Body/m_loop.htm) buscando y extrayendo las declaraciones\. Algo como esto podría valer\:

`````common-lisp
(defun split-declarations-body (body)
  (loop for expr on body
        if (and (listp (car expr))
                (eq 'declare (caar expr)))
          collect (car expr) into declarations
        else
          do (return (values declarations expr))))
`````
`````common-lisp
;; Returns
SPLIT-DECLARATIONS-BODY
`````

Esta función consigue devolver dos listas\. La primera contiene las declaraciones de ```body```\. Y la segunda el resto de expresiones\.

`````common-lisp
(let ((body '((declare (ignorable mi-fichero))
              (declare (special mi-fichero))
              (print x)
              (let ((y 5))
                (print y mi-fichero)))))

  (split-declarations-body body))
`````
`````common-lisp
;; Returns
((DECLARE (IGNORABLE MI-FICHERO)) (DECLARE (SPECIAL MI-FICHERO)))
((PRINT X)
 (LET ((Y 5))
   (PRINT Y MI-FICHERO)))
`````

Con esto ya lo tenemos todo\. La macro quedaría así\:

`````common-lisp
(defmacro with-file (var (&rest args) &body body)
  (alexandria:with-gensyms (aux-sym)
    (multiple-value-bind (declarations rest-body) (split-declarations-body body)
      `(let* ((,aux-sym (open ,@args))
              (,var ,aux-sym))
         ,@declarations           ; <-- Declaraciones
         (unwind-protect
             (progn ,@rest-body)  ; <-- El resto del cuerpo
           (close ,aux-sym))))))
`````
`````common-lisp
;; Returns
WITH-FILE
`````

Volvamos a probar el ejemplo que nos dio problemas\:

`````common-lisp
(with-file mi-fichero ("~/file.txt")
  (declare (ignore mi-fichero))
  nil)
`````
`````common-lisp
;; Returns
NIL
`````

¡Genial\! Ya funciona\. Veamos su expansión para ver cómo se queda el código\:

`````common-lisp
(macroexpand-1 '(with-file mi-fichero ("~/file.txt")
                  (declare (ignore mi-fichero))
                  (declare (special mi-fichero))
                  (print x)
                  (print y)))
`````
`````common-lisp
;; Returns
(LET* ((#:AUX-SYM656 (OPEN "~/file.txt")) (MI-FICHERO #:AUX-SYM656))
  (DECLARE (IGNORE MI-FICHERO))
  (DECLARE (SPECIAL MI-FICHERO))
  (UNWIND-PROTECT (PROGN (PRINT X) (PRINT Y)) (CLOSE #:AUX-SYM656)))
T
`````

¡Precioso\! ¿Y si no ponemos declaraciones\?

`````common-lisp
(macroexpand-1 '(with-file mi-fichero ("~/file.txt")
                  (print x)
                  (print y)))
`````
`````common-lisp
;; Returns
(LET* ((#:AUX-SYM657 (OPEN "~/file.txt")) (MI-FICHERO #:AUX-SYM657))
  (UNWIND-PROTECT (PROGN (PRINT X) (PRINT Y)) (CLOSE #:AUX-SYM657)))
T
`````

¡Espectacular\!

Antes de terminar\, vamos a modificar un poco la función ```split-declarations-body```\. De hecho\, la vamos a eliminar\. Esta función es tan común cuando se crean macros que la librería ```alexandria``` ya tiene esta función\. Y además también puede obtener el docstring si es que quisiéramos\. Así vamos a cambiar ```split-declarations-body``` por ```alexandria:parse-body```\:

`````common-lisp
(defmacro with-file (var (&rest args) &body body)
  (alexandria:with-gensyms (aux-sym)
    (multiple-value-bind (rest-body declarations) (alexandria:parse-body body)
      `(let* ((,aux-sym (open ,@args))
              (,var ,aux-sym))
         ,@declarations
         (unwind-protect
             (progn ,@rest-body)
           (close ,aux-sym))))))
`````
`````common-lisp
;; Returns
WITH-FILE
`````

Lo único que hemos tenido que cambiar es el orden de los valores de retorno ```declarations``` y  ```rest-body```\. Por lo demás\, todo se queda igual\.


<a id="TITLE:ADP-GITHUB:TAG226"></a>
## Conclusión

Seguramente haya aún algún error en nuestra macro\, pero se ha quedado lo suficientemente bien como para quedarme satisfecho\.

Las macros ```with``` parecen en un inicio inofensivas\. Macros sencillas de hacer\. Pero poco a poco uno se va dando cuenta de que siempre se pueden mejorar más y más\.

Por cierto\, no utilices en tus proyectos la macro ```with-file```\. Para eso ya existe la macro [with\-open\-file](http://www.lispworks.com/reference/HyperSpec/Body/m_w_open.htm) que está en el propio estándar\.

Y si crees que las macros ```with``` acaban aquí\, te equivocas\. Hay otras macros ```with``` que funcionan de manera diferente\, que no bindean objetos sino que sirven para facilitar el acceso a cierta información\. Un ejemplo es [with\-slots](http://www.lispworks.com/reference/HyperSpec/Body/m_w_slts.htm)\, que permite acceder a cada uno de los miembros de una clase o estructura de manera muy sencilla\.

Y por último\, cuando vayas viendo una y otra macro ```with```\, verás que todas tienen cosas en común\. Son muy parecidas\. Así que\.\.\. ¿no se podrá hacer algún tipo de abstracción\? ¿No será posible crear una macro ```with``` general\? La respuesta es sí\. Hay varias librerías que intentan esto\, aunque no son de mi agrado\. Por eso creé la mía propia\. No es perfecta\, pero hace lo que quiero\. La librería es [clith](https://github.com/HectareaGalbis/clith)\.

Y nada más\, ya he dicho todo lo que quería contar sobre las macros ```with```\. O al menos una parte de ellas claro\.

\:D