
(in-package :adpgh)

@title[:toc nil]{Las macros with}

@table-of-contents[]

@subtitle{Introducción}

La familia de macros @code{with} es sin duda una de las más conocidas en Common Lisp. Y es que a pesar de que existe el @italic{Garbage Collector} existen objetos que requieren de una inicialización y una terminación.

El ejemplo más claro es el uso de ficheros. Para poder leer o escribir sobre un fichero hay que abrirlo. Y cuando ya hemos terminado, debemos cerrarlo.

@code-block[:lang "common-lisp"]{
(let ((file (open "~/file.txt")))
  ;; Leemos o escribimos en el fichero
  (close file)) ; Cerramos el fichero
}

Pero esto tiene un problema. Se nos puede olvidar cerrarlo. Parece una tontería, pero pasa mucho.

Además, también podemos ver que hay redundancia de información. Estamos usando @clref[let] para crear una variable local @code{file} con el objeto que representa al archivo abierto. Lo más común es que sólo lo usemos dentro de este @clref[let], y si no hacemos ninguna asignación ni siquiera será posible usar este fichero desde fuera al no tener acceso a la variable @code{file}. Así que, ¿no debería ser obvio que el fichero tiene que cerrarse al acabar el @clref[let]?

La expresión @clref[let] no es capaz de averiguar esto por sí sólo. ¡Y tampoco debe! Aunque lo normal es usar el fichero sólo dentro del @clref[let] aún podemos asignar el fichero a una variable externa y cerrar el fichero más tarde.

El concepto de que algo vive de forma limitada en Common Lisp se conoce como @bold{dynamic extent}. En el ejemplo anterior hemos creado un fichero con @italic{dynamic extent}. El fichero vive de manera limitada durante la ejecución de la expresión @clref[let]. Sin embargo, el hecho de que tenga @italic{dynamic extent} es expresado de manera explícita mediante el uso de la función @clref[close]. Y es claro que un archivo abierto con @clref[open] debe ser cerrado con @clref[close], por lo que el uso de esta función es ciertamente redundante.

Para estos casos se usan las macros @code{with}. Una macro @code{with}, en general, va a definir un objeto con @italic{dynamic extent}. Es decir, va a definir un objeto que estará disponible durante un tiempo limitado. De hecho, una buena macro @code{with} debe asegurarse de que el objeto en cuestión es finalizado sí o sí, independientemente de si la ejecución ha sido correcta o ha ocurrido algún error. Dicho de otra forma, si salimos de la expresión @code{with}, el objeto que haya definido debe finalizarse sin falta.


@subtitle{Definiendo una macro with}

Siguiendo el ejemplo del fichero, vamos a crear una macro que llamaremos @code{with-file} que abra un fichero y lo cierre automáticamente al terminarse la macro.

La macro debe saber cómo abrir el fichero a qué variable debe asignarle el objeto que representa el fichero. Así que al menos debe recibir un símbolo para la variable y los argumentos que le pasaremos luego a la función @clref[open]. No nos olvidemos de las expresiones que se tienen que ejecutar teniendo el fichero abierto.

Una primera versión de nuestra macro @code{with-file} podría ser esta:

@example|{
(defmacro with-file (var (&rest args) &body body)
  `(let ((,var (open ,@args)))
     ,@body
     (close ,var)))
}|

La macro devuelve exactamente el código del ejemplo. Se crea un fichero, lo usamos en el cuerpo de la macro, y finalmente se cierra. Suena bien.

@example{
(with-file mi-fichero ("~/file.txt" :direction :output :if-does-not-exist :create :if-exists :supersede)
  (prin1 "Hola mundo" mi-fichero))

(with-file mi-fichero ("~/file.txt")
  (read mi-fichero))
}

¡Genial! Parece que... Espera un momento... Primero he escrito @code{"Hola mundo"} en el fichero @code{~/file.txt} (lo he comprobado en mi pc). Y luego he devuelto lo que devolvía @code{(read mi-fichero)} que debería ser @code{"Hola mundo"} de nuevo. Sin embargo, se ha devuelto el valor @code{T}.

Vamos a debuguear. Vamos a expandir la macro cuando estamos leyendo a ver si encontramos algo raro.

@example{
(macroexpand-1 '(with-file mi-fichero ("~/file.txt")
                  (read mi-fichero)))
}

¡Ajá! Por un momento he pensado que se devolvía @code{(read mi-fichero)} pero la última expresión es la llamada a la función @clref[close]. Y esta función devuelve @code{T} si el stream recibido (en este caso el fichero) estaba abierto. Todo cuadra.

Pues toca arreglar nuestra macro para que pueda devolver la última expresión. Podría ser algo así:

@example|{
(defmacro with-file (var (&rest args) &body body)
  (alexandria:with-gensyms (result-sym)
    `(let* ((,var (open ,@args))
            (,result-sym (progn ,@body)))
       (close ,var)
       ,result-sym)))
}|

Aquí estoy usando @code{alexandria:with-gensyms} para crear un símbolo no internado para que nuestra macro sea @link[:address "https://en.wikipedia.org/wiki/Hygienic_macro"]{higiénica}. Por otro lado, observa que se está usando @clref[let*] en lugar de @clref[let]. Esto es para que las expresiones en @code{body} tengan acceso a la variable @code{,var}.

Probemos de nuevo a leer el fichero @code{"~/file.txt"}

@example{
(with-file mi-fichero ("~/file.txt")
  (read mi-fichero))
}

¡Ahora sí! Aunque por si acaso, vamos a comprobar que realmente se cierra el fichero. Vamos a crear una variable fuera de la expresión @code{with-file} y le asignaremos el fichero que se ha abierto. Una vez finalizada la expresión @code{with-file} la función @clref[open-stream-p] debe devolver @code{NIL}.

@example[:results :output]{
(let (mi-fichero-externo)
  (with-file mi-fichero ("~/file.txt")
    (format t "Dentro de WITH-FILE: ~a~%" (open-stream-p mi-fichero))
    (setf mi-fichero-externo mi-fichero))
  (format t "Fuera de WITH-FILE: ~a" (open-stream-p mi-fichero-externo)))
}

¡Perfecto!

Pues no se tú, pero yo creo que la macro está ya perfecta, ¿verdad...?

@example[:results :output]{
(let (mi-fichero-externo)
  (ignore-errors (with-file mi-fichero ("~/file.txt")
                   (setf mi-fichero-externo mi-fichero)
                   (error "Oh no!!")))
  (format t "Fuera de WITH-FILE: ~a" (open-stream-p mi-fichero-externo))) ; ¡¡¡Sigue abierto!!!
}

¡Oh no! ¡En este ejemplo al lanzarse una excepción el fichero no se ha cerrado!


@subtitle{El operador @code{unwind-protect}}

Tras el error anterior es tentador pensar en alguna solución que involucre capturar la excepción y relanzarla tras haber cerrado el fichero. Pero hay otras expresiones que pueden sacarnos de la macro @code{with-file} sin haber cerrado el fichero, como por ejemplo, @clref[return-from].

Pensar en todos los casos posibles donde podemos salir de @code{with-file} de manera no controlada para tratar de tomar el control es simplemente inviable.

En lugar de eso, podemos usar un operador que ya existe en el estándar de Common Lisp y que hace justamente lo que queremos. Hablamos de @clref[unwind-protect].

Según el estándar @clref[unwind-protect] hace lo siguiente:

@quoted{
@bold{unwind-protect} @italic{protected-form cleanup-form* => result*}

@bold{unwind-protect} evaluates @italic{protected-form} and guarantees that @italic{cleanup-forms} are executed before @bold{unwind-protect} exits, whether it terminates normally or is aborted by a control transfer of some kind.
}

Es decir, evalúa la expresión que le pasemos como primer argumento y se asegura de que el resto de argumentos se evalúen tras evaluar el primer argumento. Podemos ver algún ejemplo:

@example{
(unwind-protect
    (print "Hola")
  (print "Adios"))
}

Por ahora nada especial. Primero se evalúa @code{(print "Hola")} y luego @code{(print "Adios")}. Aunque sí cabe destacar que el valor devuelto por @clref[unwind-protect] es el valor del primer argumento. En este caso se devuelve el valor de @code{(print "Hola")} que es precisamente @code{"Hola"}.

Probemos a usar una excepción:

@example[:results :output]{
(ignore-errors
  (unwind-protect
      (progn (print "Hola")
             (error "Oh no!"))
    (print "Adios")))
}

¡Genial! También funciona.

De hecho, si quitamos @clref[unwind-protect] veremos que @code{"Adios"} no se imprime:

@example[:results :output]{
(ignore-errors
  (print "Hola")
  (error "Oh no!")
  (print "Adios"))
}

Pues ya toca usar @clref[unwind-protect] en nuestra macro @code{with-file}. La macro quedaría así:

@example|{
(defmacro with-file (var (&rest args) &body body)
  `(let* ((,var (open ,@args)))
     (unwind-protect
         (progn ,@body)
       (close ,var))))
}|

Recuerda que en la anterior versión de la macro @code{with-file} guardábamos el resultado en una variable auxiliar. Ahora ya no hace falta pues @clref[unwind-protect] devolverá lo que devuelva su primer argumento, que en nuestro caso es todo el cuerpo. El segundo argumento de @clref[unwind-protect] es @code{(close ,var)}, de manera que siempre se cerrará el fichero cuando salgamos abruptamente de la macro @code{with-file}.

@example[:results :output]{
(let (mi-fichero-externo)
  (ignore-errors (with-file mi-fichero ("~/file.txt")
                   (setf mi-fichero-externo mi-fichero)
                   (error "Oh no!!")))
  (format t "Fuera de WITH-FILE: ~a" (open-stream-p mi-fichero-externo))) ; ¡Ahora sí se cierra!
}

Ahora sí, ¿verdad? Yo creo que nuestra macro @code{with-file} ya está perfectísima. Bueno...

@code-block[:lang "common-lisp"]{
(with-file mi-fichero ("~/file.txt")
  (setf mi-fichero 3))
}

@code-block[:lang "common-lisp"]{
The value
  3
is not of type
  STREAM
   [Condition of type TYPE-ERROR]
}


@subtitle{Una variable auxiliar}

Quizás estés pensando que lo anterior es algo forzado. Estamos asignando un valor a @code{mi-fichero} para que al usarse @clref[close] se produzca un error. Pero hay que recordar que la premisa de una macro @code{with} es que el objeto dure tanto como la propia expresión. Y al producirse el error en @clref[close] estamos saliendo de la macro (de manera abrupta) sin que el fichero se haya cerrado. Por tanto, la macro sigue sin estar perfecta.

El problema está en que estamos usando la variable elegida por quien use la macro @code{with-file} para cerrar el fichero. Y quien use la macro @code{with-file} puede hacer lo que quiera, incluido asignar un nuevo valor a la variable.

Así que la solución es sencilla. Usemos una variable diferente que no se pueda usar desde fuera. Es decir, un símbolo no internado. Cuando abramos el fichero asignaremos el objeto a dos variables diferentes. Una la pasada como argumento y otra interna creada por la propia macro.

La macro quedaría así:

@example|{
(defmacro with-file (var (&rest args) &body body)
  (alexandria:with-gensyms (aux-sym)
    `(let* ((,var (open ,@args))
            (,aux-sym ,var))
       (unwind-protect
           (progn ,@body)
         (close ,aux-sym)))))
}|

¡Listo! No ha sido tan difícil. Probemos de nuevo el ejemplo:

@example{
(with-file mi-fichero ("~/file.txt")
  (setf mi-fichero 3))
}

¡Genial! Ya no da error. Pero asegurémonos de que se cierra el fichero:

@example[:results :output]{
(let (mi-fichero-externo)  
  (with-file mi-fichero ("~/file.txt")
    (setf mi-fichero-externo mi-fichero)
    (setf mi-fichero 3))
  (format t "Fuera de WITH-FILE: ~a" (open-stream-p mi-fichero-externo)))
}

¡Perfecto! Aunque la macro aún no...


@example{
(with-file mi-fichero ("~/file.txt")
  nil)
}

¿Ves el problema? ¿No? Pues ahí está precisamente lo malo.


@subtitle{Un detalle sutil}

Quizás lo siguiente pueda parecer innecesario, pero a mi me gusta que todo esté lo más perfecto posible. También es cierto que toda esta sección depende de la implementación que estés usando, pues los mensajes de warning o errores pueden variar de una a otra. En mi caso estoy usando SBCL.

Volvamos a ver el ejemplo anterior:

@example{
(with-file mi-fichero ("~/file.txt")
  nil)
}

Parece normal, pero observa ahora qué ocurre con una expresión @clref[let]:

@example{
(let (mi-fichero)
  nil)
}

@code-block[:lang "common-lisp"]{
;; Warning
in: PROGN (LET (MI-FICHERO)
         NIL)
    (LET (ADP-GITHUB::MI-FICHERO)
      NIL)

caught STYLE-WARNING:
  The variable MI-FICHERO is defined but never used.
}

¡Ajá! Ahora sí se ve el problema. La expresión @clref[let] nos avisa de que no estamos usando la variable @code{mi-fichero}. Sin embargo, nuestra macro @code{with-file} no lo ha hecho.

Veamos el porqué expandiendo el ejemplo:

@example{
(macroexpand-1 '(with-file mi-fichero ("~/file.txt")
                  nil))
}

Observa que @code{mi-fichero} se define con la función @clref[open]. Y luego se define la variable @code{#:AUX-SYM}... ¡usando la variable @code{mi-fichero}! Por eso no estamos recibiendo el warning, porque siempre se está usando.

Pero el arreglo es sencillo, podemos definir las variables en el orden contrario.

@example|{
(defmacro with-file (var (&rest args) &body body)
  (alexandria:with-gensyms (aux-sym)
    `(let* ((,aux-sym (open ,@args)) ; <- Primero aux-sym
            (,var ,aux-sym))         ; <- Segundo var
       (unwind-protect
           (progn ,@body)
         (close ,aux-sym)))))
}|

Ahora sí deberíamos recibir el warning:

@example{
(with-file mi-fichero ("~/file.txt")
  nil)
}

@code-block[:lang "common-lisp"]{
;; Warning
in: PROGN (WITH-FILE MI-FICHERO
           ("~/file.txt")
         NIL)
    (ADP-GITHUB::MI-FICHERO #:AUX-SYM624)

caught STYLE-WARNING:
  The variable MI-FICHERO is defined but never used.
}

¡Ahora sí! Y ahora que sí nos avisa, si realmente queremos que no nos lance el warning podemos usar una declaración como @code{(declare (ignore mi-fichero))}:

@code-block[:lang "common-lisp"]{
(with-file mi-fichero ("~/file.txt")
  (declare (ignore mi-fichero))
  nil)
}

@code-block[:lang "common-lisp"]{
;; Error
caught ERROR:
  There is no function named DECLARE.
}

¡Pero será hijo de ...!


@subtitle{Añadiendo declaraciones}

Bueno, al igual que en los anteriores casos, vamos a expandir el ejemplo para ver mejor porqué obtenemos un error al usar una declaración:

@example{
(macroexpand-1 '(with-file mi-fichero ("~/file.txt")
                  (declare (ignore mi-fichero))
                  nil))
}

Fíjate que la declaración está justo dentro de la expresión @clref[progn]. Y claro, esta expresión no acepta declaraciones. Es más, las declaraciones que hagamos van estar relacionadas con el símbolo @code{mi-fichero}. Es decir, las declaraciones deberían situarse justo al empezar el cuerpo de la expresión @clref[let].

@code-block[:lang "common-lisp"]|{
(defmacro with-file (var (&rest args) &body body)
  (alexandria:with-gensyms (aux-sym)
    `(let* ((,aux-sym (open ,@args))
            (,var ,aux-sym))
                                ; <-- Aquí deberían ir las declaraciones
       (unwind-protect
           (progn ,@body)
         (close ,aux-sym)))))
}|

Normalmente las declaraciones estarán en el propio cuerpo de la macro @code{with-file}, es decir, la variable @code{body}. Una opción podría ser recibir un nuevo argumento justo antes de @code{body}:

@code-block[:lang "common-lisp"]{
(defmacro with-file (var (&rest args) declaration &body body)
  ...
  )
}

Pero las declaraciones son opcionales. Y además, todas las expresiones que aceptan declaraciones suelen permitir varias líneas de declaraciones. Es decir, que lo siguiente debería ser válido:

@code-block[:lang "common-lisp"]{
(with-file mi-fichero ("~/file.txt")
  (declare (special mi-fichero))
  (declare (ignorable mi-fichero))
  ...
  )
}

No hay más remedio que extraer las declaraciones de la variable @code{body}. Basta con realizar un @clref[loop] buscando y extrayendo las declaraciones. Algo como esto podría valer:

@example{
(defun split-declarations-body (body)
  (loop for expr on body
        if (and (listp (car expr))
                (eq 'declare (caar expr)))
          collect (car expr) into declarations
        else
          do (return (values declarations expr))))
}

Esta función consigue devolver dos listas. La primera contiene las declaraciones de @code{body}. Y la segunda el resto de expresiones.

@example{
(let ((body '((declare (ignorable mi-fichero))
              (declare (special mi-fichero))
              (print x)
              (let ((y 5))
                (print y mi-fichero)))))

  (split-declarations-body body))
}

Con esto ya lo tenemos todo. La macro quedaría así:

@example|{
(defmacro with-file (var (&rest args) &body body)
  (alexandria:with-gensyms (aux-sym)
    (multiple-value-bind (declarations rest-body) (split-declarations-body body)
      `(let* ((,aux-sym (open ,@args))
              (,var ,aux-sym))
         ,@declarations           ; <-- Declaraciones
         (unwind-protect
             (progn ,@rest-body)  ; <-- El resto del cuerpo
           (close ,aux-sym))))))
}|

Volvamos a probar el ejemplo que nos dio problemas:

@example{
(with-file mi-fichero ("~/file.txt")
  (declare (ignore mi-fichero))
  nil)
}

¡Genial! Ya funciona. Veamos su expansión para ver cómo se queda el código:

@example{
(macroexpand-1 '(with-file mi-fichero ("~/file.txt")
                  (declare (ignore mi-fichero))
                  (declare (special mi-fichero))
                  (print x)
                  (print y)))
}

¡Precioso! ¿Y si no ponemos declaraciones?

@example{
(macroexpand-1 '(with-file mi-fichero ("~/file.txt")
                  (print x)
                  (print y)))
}

¡Espectacular!

Antes de terminar, vamos a modificar un poco la función @code{split-declarations-body}. De hecho, la vamos a eliminar. Esta función es tan común cuando se crean macros que la librería @code{alexandria} ya tiene esta función. Y además también puede obtener el docstring si es que quisiéramos. Así vamos a cambiar @code{split-declarations-body} por @code{alexandria:parse-body}:

@example|{
(defmacro with-file (var (&rest args) &body body)
  (alexandria:with-gensyms (aux-sym)
    (multiple-value-bind (rest-body declarations) (alexandria:parse-body body)
      `(let* ((,aux-sym (open ,@args))
              (,var ,aux-sym))
         ,@declarations
         (unwind-protect
             (progn ,@rest-body)
           (close ,aux-sym))))))
}|

Lo único que hemos tenido que cambiar es el orden de los valores de retorno @code{declarations} y  @code{rest-body}. Por lo demás, todo se queda igual.


@subtitle{Conclusión}

Seguramente nuestra macro @code{with-file} se pueda mejorar aún más, pero se ha quedado lo suficientemente bien como para quedarme satisfecho.

Las macros @code{with} parecen en un inicio inofensivas. Macros sencillas de hacer. Pero poco a poco uno se va dando cuenta de que siempre se pueden mejorar más y más.

Por cierto, no utilices en tus proyectos la macro @code{with-file}. Para eso ya existe la macro @clref[with-open-file] que está en el propio estándar.

Y si crees que las macros @code{with} acaban aquí, te equivocas. Hay otras macros @code{with} que funcionan de manera diferente, que no bindean objetos sino que sirven para facilitar el acceso a cierta información. Un ejemplo es @clref[with-slots], que permite acceder a cada uno de los miembros de una clase o estructura de manera muy sencilla.

Y por último, cuando vayas viendo una y otra macro @code{with}, verás que todas tienen cosas en común. Son muy parecidas. Así que... ¿no se podrá hacer algún tipo de abstracción? ¿No será posible crear una macro @code{with} general? La respuesta es sí. Hay varias librerías que intentan esto, aunque no son de mi agrado. Por eso creé la mía propia. No es perfecta, pero hace lo que quiero. La librería es @link[:address "https://github.com/HectareaGalbis/clith"]{clith}.

Y nada más, ya he dicho todo lo que quería contar sobre las macros @code{with}. O al menos una parte de ellas claro.

:D
