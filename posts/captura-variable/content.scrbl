
(in-package #:lispylambda)


@title[:tag captura-variable :toc nil]{Macros y la captura de variable}

Las macros de Common Lisp dan una libertad increible. Pero esta libertad viene con un precio. Si no tenemos cuidado, podemos introducir bugs difíciles de detectar que nos darán dolor de cabeza durante unas buenas horas. Estos bugs suelen estar ocasionados por la llamada @bold{captura de variable}.

@table-of-contents[]

@subtitle{¿Qué es la captura de variable?}

Consideremos el ejemplo de crear una macro @code{swap}. Debe recibir dos argumentos e intercambiar sus valores. Pensemos primero en el código al que debe expandirse. Si queremos intercambiar el valor de dos variables lo haríamos de esta manera:

@example{
(let ((a "a") (b "b"))

  (let ((aux a))
    (setf a b)
    (setf b aux))

  (format nil "a: ~a | b: ~a" a b))
}

Como se puede ver, el código al que debe expandirse nuestra macro es:

@code-block[:lang "common-lisp"]{
(let ((aux a))
  (setf a b)
  (setf b aux))
}

La macro @code{swap} que estamos buscando podría ser perfectamente la siguiente:

@example{
(defmacro swap (a b)
  `(let ((aux ,a))
     (setf ,a ,b)
     (setf ,b aux)))
}

A primera vista, parece que esté perfecta. De hecho, la mayoría de veces funcionará sin problemas.

@example{
(let ((a "a") (b "b"))
  (swap a b)
  (format nil "a: ~a | b: ~a" a b))
}

Pero tarde o temprano, los bugs acaban por manifestarse.

@example{
(let ((a "a") (aux 5))
  (swap a aux)
  (format nil "a: ~a | aux: ~a" a aux))
}

Observa que @code{swap} no ha hecho absolutamente nada. Las variables @code{a} y @code{aux} están intactas. Evidentemente el ejemplo sugiere que el problema está al haber introducido la variable @code{aux}. Y casualmente es la misma variable que hemos usado en la definición de la macro @code{swap}. Para despejar las dudas, vamos a expandir la anterior llamada de @code{swap}.

@example{
(macroexpand-1 '(swap a aux))
}

Analicemos detenidamente el ejemplo con el código expandido.

@code-block[:lang "common-lisp"]{
(let ((a "a") (aux 5))
  (let ((aux a))
    (setf a aux)
    (setf aux aux)))
}

En un inicio tenemos que @code{a} vale @code{"a"} y que @code{aux} vale @code{5}. En el @code{let} interno se vuelve a asignar un nuevo valor para @code{aux}, en este caso el valor de la variable @code{a}, es decir, @code{"a"}. Hay que tener en cuenta que aquí aún no se ha hecho ninguna asignación. Las variables del @code{let} externo siguen teniendo los mismos valores. Con el @code{let} interno se está @italic{declarando} una nueva variable @code{aux}. Se dice que el @code{aux} del @code{let} interno está ensombreciendo al @code{aux} del @code{let} externo. Dentro del @code{let} interno tenemos entonces las variables @code{a} y @code{aux} que contienen el mismo valor @code{"a"}. Además, hemos perdido el acceso al valor original de la variable @code{aux} del @code{let} externo. Ya no tenemos acceso al valor @code{5}, por lo que nunca podremos asignárselo a la variable @code{a}. Por tanto, a la variable @code{a} se le asigna el valor @code{"a"} que contiene el @code{aux} interno. Y a la variable @code{aux}, que sigue siendo la variable del @code{let} interno se le asigna el valor @code{"a"}, es decir, lo que ya tenía.

En resumen, a la variable @code{a} se le asigna el valor que ya tenía y a la variable @code{aux} no se le asigna nada porque es ensombrecida por un @code{let} interno.

El problema de nuestra macro es que ha permitido que tanto el argumento como la variable auxiliar utilicen la misma variable que en este caso era la variable @code{aux}. Se ha producido una @bold{colisión de nombres}.

Es esta @bold{colisión de nombres} lo que denominamos @bold{captura de variable}.


@subtitle{Evitando la captura de variable}

Tras saber que una captura de variable es una colisión de nombres la pregunta es obvia: ¿Cómo evitamos la colisión de nombres? Y la respuesta parece obvia: Necesitamos usar variables en nuestra macro que siempre vayan a ser diferentes a cualquier argumento que nos puedan pasar.

Si estuviesemos hablando de las macros del lenguage C o C++, diríamos que basta con usar una variable con un nombre lo suficientemente raro. Eso bajaría las problabilidades de colisión de nombres, pero aún puede darse la posibilidad. Por suerte estamos en Common Lisp, un lenguaje infinitamente superior. Y si el lenguaje nos permite hacer las cosas bien, no deberíamos limitarnos a usar un nombre lo suficientemente raro.

Hasta ahora hemos hablado de colisión de nombres pero recordemos que en Common Lisp usamos símbolos. Así que lo buscamos realmente es evitar la colisión de símbolos. Así que la pregunta que sigue realmente necesitamos hacer es: ¿Qué símbolos son siempre diferentes a cualquier argumento que reciba nuestra macro?

Para entender bien la respuesta, pensemos en cómo se comparan los símbolos. Recordemos que los símbolos son en realidad objetos. En particular, los símbolos contienen un nombre y pertenecen a un paquete.

@example{
(symbol-name 'aux)
}

@example{
(symbol-package 'aux)
}

En este caso el símbolo @code{aux} tiene de nombre @code{"AUX"} y pertenece al paquete @code{"LISPYLAMBDA"}.

A la hora de comparar si dos símbolos son iguales, Common Lisp comprueba si son literalmente el mismo objeto. La clave está en que cuando escribimos @code{'aux}, Common Lisp entiende que queremos el símbolo con nombre @code{"AUX"} y que pertenece, en este caso, al paquete actual (@code{"LISPYLAMBDA"}). Así que inmediatamente se va al paquete @code{"LISPYLAMBDA"} y busca algún símbolo cuyo nombre sea @code{"AUX"}. Si lo encuentra, devuelve el símbolo, o dicho de otra forma, el objeto de tipo símbolo.

Sabiendo esto, podemos ver que a la hora de comparar dos símbolos siempre estarán involucrados tanto el nombre como el paquete de cada símbolo. En particular, dos símbolos serán iguales si tanto su nombre como su paquete son el mismo. Esto nos da las dos siguientes opciones en nuestra búsqueda del símbolo perfecto para nuestra macro @code{swap}:

@itemize[
  @item{Buscar un símbolo con algún nombre especial.}
  @item{Buscar un símbolo con algún paquete especial.}
]

Suponiendo que los paquetes son iguales, es claro que si un símbolo tiene un nombre en específico, nuestra macro @code{swap} siempre podría recibir dicho símbolo con dicho nombre. Por ejemplo, si usamos un símbolo con nombre @code{"__lcj2w78fh73x3  NQ93acÑ_wclnc   qwola"}, siempre podríamos recibirlo como argumento, aunque sea muy raro. Y sí, un símbolo con ese nombre puede existir.

@example{
(symbol-name '__\l\c\j2\w78\f\h73\x3\ \ NQ93\a\cÑ_\w\c\l\n\c\ \ \ \q\w\o\l\a)
}

Sólo nos queda entonces la segunda opción. Necesitamos centrar nuestra búsqueda en los paquetes.

Decíamos que cuando escribimos un símbolo, Common Lisp busca en el paquete correspondiente un símbolo con el nombre del símbolo a buscar. Pero, ¿y si el símbolo no pertenece a ningún paquete? De esta forma, Common Lisp nunca podría encontrarlo a partir de un nombre. Common Lisp busca siempre en el paquete correspondiente, ¡pero el símbolo no está en ninguno!

Recordemos que los símbolos que no pertenecen a ningún paquetes se denominan símbolos no internados y podemos crear estos símbolos de forma muy sencilla.

Una de las formas más sencillas es usar el prefijo de paquete @code{#:}.

@example{
(symbol-package '#:hey)
}

Al ser un símbolo no internado, su paquete es @code{NIL}. Es decir, no pertenece a ningún paquete.

Para estar seguros de que este es el tipo de símbolos que necesitamos podemos crear una macro que compruebe que el argumento sea igual al símbolo no internado que usemos.

@example{
(defmacro arg-eq-p (sym)
  `(eq '#:aux ',sym))
}

Probemos.

@example{
(arg-eq-p a)
}

@example{
(arg-eq-p aux)
}

@example{
(arg-eq-p #:aux)
}

Observa que en cada uno de los ejemplos se verifica que el argumento nunca es igual al símbolo no internado @code{#:aux} de la macro @code{arg-eq-p}. Y esto también ocurre en el último ejemplo. De hecho, no hacía falta crear ninguna macro para comprobar esto:

@example{
(eq '#:aux '#:aux)
}

Aunque parezca que son el mismo símbolo por tener la misma representación gráfica, no lo son en absoluto. Este comportamiento está especificado en @link[:address "https://www.lispworks.com/documentation/HyperSpec/Body/02_dhe.htm"]{la sección 2.4.8.5 del hyperspec}:

@quoted{
#: introduces an uninterned symbol whose name is symbol-name.
Every time this syntax is encountered, a distinct uninterned symbol is created.
}

Cada vez que escribamos @code{#:un-simbolo} se creará un nuevo símbolo internado diferente. Por eso en nuestro ejemplo se indica que son diferentes, porque al escribir dos veces @code{#:aux} tenemos en total dos símbolos no internados con el nombre @code{"AUX"}.

Con esto ya podemos hacer nuestra macro. Un primer intento podría ser el siguiente.

@example{
(defmacro swap (a b)
  `(let ((#:aux ,a))
     (setf ,a ,b)
     (setf ,b #:aux)))
}

¡Pero recuerda! Cada vez que escribamos @code{#:aux} se va a crear un nuevo símbolo. Por tanto los dos símbolos @code{#:aux} de nuestra macro son diferentes. ¿Cómo solucionamos esto? Basta guardar el símbolo no internado en una variable.

@example{
(defmacro swap (a b)
  (let ((aux-sym '#:aux))
    `(let ((,aux-sym ,a))
       (setf ,a ,b)
       (setf ,b ,aux-sym))))
}

Ahora si. Observa que estamos guardando el símbolo no internado en la variable @code{aux-sym}. Seguidamente, creamos el código que vamos a devolver. Cada vez que queramos usar la variable no internada basta con obtenerla evaluando la variable @code{aux-sym}. De ahí que se esté usando la coma con @code{aux-sym}. 

Si probamos ahora, la macro ya debe funcionar perfectamente:

@example{
(let ((a "a") (aux 5))
  (swap a aux)
  (format nil "a: ~s | aux: ~s" a aux))
}

¡Genial!

@subtitle{¿Cuándo debo usar un símbolo no internado?}

La regla de oro consiste en usar un símbolo no internado siempre que necesitemos alguna variable auxiliar como en el caso de @code{swap}. Más precisamente, necesitamos este tipo de símbolos cada vez que se va a hacer una ligadura que sea interna, es decir, que desde fuera no se deba usar.

Para la macro @code{swap} necesitábamos una variable auxiliar @code{#:aux} para poder realizar el intercambio de valores. En este caso era fácil identificar la ligadura porque la estamos creando de manera explícita al usar @clref[let]. Sin embargo, otras veces no es tan obvio pues no siempre estas variables se definen con un @clref[let] o un @clref[multiple-value-bind].

Supongamos que queremos una macro que nos permita repetir varias veces la ejecución de una o varias expresiones. La manera más sencilla de hacer esto es usar la macro @clref[dotimes].

@example{
(dotimes (aux 5)
  (princ "Hola")
  (terpri) ; Nueva linea
  )
}

Buscamos el mismo comportamiento pero sin tener que especificar una variable como @code{aux}. Sólo queremos indicar el número y las expresiones. Una opción sería esta:

@example|{
(defmacro repeat (num &body exprs)
  `(dotimes (aux ,num)
     ,@exprs))
}|

La forma de usarla es sencilla:

@example{
(repeat 10
  (princ "Hola mundo")
  (terpri))
}

Pero claro, internamente la macro @clref[dotimes] bindea la variable @code{aux} con un valor del 0 al 9 para cada iteración del bucle. Si utilizásemos una variable @code{aux} el resultado podría no ser el esperado:

@example{
(let ((aux 5))
  (repeat 10
    (format t "aux vale: ~a" aux)
    (terpri)))
}

El resultado esperado es que siempre imprima @code{aux vale: 5}, pero como @clref[dotimes] bindea nuevos valores a la variable @code{aux} en cada iteración, ocurre el desastre.

La solución ya la sabemos, usar un símbolo no internado:

@example|{
(defmacro repeat (num &body exprs)
  (let ((aux '#:aux))
    `(dotimes (,aux ,num)
       ,@exprs)))
}|

Como ahora el simbolo usado es no internado, todo funciona perfectamente:

@example{
(let ((aux 5))
  (repeat 10
    (format t "aux vale: ~a" aux)
    (terpri)))
}

¡Perfecto!

@subtitle{Gensym}

En la práctica, la macro se puede considerar perfecta. Ya no fallará nunca. Está libre de bugs. Pero hay un pequeño detalle que nos puede jugar una mala pasada. Estas macros son pequeñas, pero en un proyecto real las macros pueden ser muy grandes, por lo que siempre acabaremos recurriendo a algún sistema de debugueo. En particular, la herramienta más usada es @clref[macroexpand-1] o @clref[macroexpand].

Imaginemos que tenemos un código como el siguiente:

@code-block[:lang "common-lisp"]{
(let ((a "a") (b "b") (c "c"))
  (repeat 5
    (swap a b)
    (swap a c)
    (swap b c)))
}

Supongamos que no está haciendo lo que esperamos, así que decidimos expandir las macros @code{repeat} y @code{swap}:

@example{
(macroexpand-1 '(repeat 5
                  (swap a b)
                  (swap a c)
                  (swap b c)))
}

Nos interesa también expandir la llamada a @code{swap}. Así que voy a hacer lo siguiente:


@example{
(macroexpand-1 `(repeat 5
                  ,(macroexpand-1 '(swap a b))
                  ,(macroexpand-1 '(swap a c))
                  ,(macroexpand-1 '(swap b c))))
}

Recordemos que ya sabemos que las macros usando símbolos no internados son correctas. Pero hay un claro problema aquí. Ya se hace difícil distinguir entre las diferentes variables @code{#:aux}. Y aunque hayamos usado un nombre diferente para la macro @code{repeat}, piensa que @code{swap} está generando 3 variable @code{#:aux} que son diferentes. Este caso es pequeño, pero a medida que crece un proyecto, esto puede dificultar bastante la búsqueda de bugs.

Recapitulemos qué tenemos y qué necesitamos ahora. Hemos visto que necesitamos símbolos no internados para nuestras macros. Pero ahora también queremos que sus nombres sean diferentes para poder diferenciarlos a la hora de debuguear.

Por suerte, los diseñadores de Common Lisp pensaron en todo y nos dieron la función @clref[cl:gensym].

Primero, lo importante. Si nos fijamos en la documentación encontramos esta frase:

@quoted{
Creates and returns a fresh, uninterned symbol, ...
}

Bien, crea un símbolo no internado. Y lo segundo, es que está hecho para asegurar que cada vez consigamos un nombre diferente. Basta ver el siguiente ejemplo para entender cómo funciona:

@example{
(loop for i from 0 below 10
      collect (gensym "AUX"))
}

Acabamos de crear una lista con 10 símbolos no internados. Observa que cada símbolo tiene añadido un sufijo. Este sufijo es un número que irá aumentando de 1 en 1 cada vez que se llame a @clref[cl:gensym]. En particular, se aumenta en 1 la variable @clref[*gensym-counter*].

Es tan simple el funcionamiento, que nada te impide modificar la variable o indicar un prefijo para intentar que dos variables no internadas acaben con el mismo nombre. Es decir, el método no es perfecto.

@example{
(list
  (let ((*gensym-counter* 999))
    (gensym "HEY"))
  (let ((*gensym-counter* 999))
    (gensym "HEY")))
}

Al ser estos símbolos no internados, Common Lisp no tiene una forma directa de saber qué símbolos ha creado anteriormente. Y aunque una opción posible sería ir guardándolos en algún contenedor, con el tiempo este contenedor se haría enorme ocupando un espacio de memoria valioso.

Como el programa va a funcionar perfectamente, podemos al menos sacrificar que los nombres no siempre vayan a ser diferentes.

Aunque ya te puedo asegurar que en prácticamente todo el tiempo que le dediques a debuguear macros (y si no has modificado la variable @clref[*gensym-counter*]) nunca te vas a encontrar con el remoto caso de que dos símbolos no internados diferentes acaben con el mismo nombre.

Dicho esto, modifiquemos nuestras macros @code{swap} y @code{repeat}:

@example{
(defmacro swap (a b)
  (let ((aux (gensym "AUX")))
    `(let ((,aux ,a))
       (setf ,a ,b)
       (setf ,b ,aux))))
}

@example|{
(defmacro repeat (num &body exprs)
  (let ((aux (gensym "AUX")))
    `(dotimes (,aux ,num)
       ,@exprs)))
}|

Y por último, veamos la expansión total del ejemplo de más arriba:

@example{
(macroexpand-1 `(repeat 5
                  ,(macroexpand-1 '(swap a b))
                  ,(macroexpand-1 '(swap a c))
                  ,(macroexpand-1 '(swap b c))))
}

Ahora sí. Mucho mejor. Obviamente no es el código más legible, pero al menos podemos distinguir las diferentes variables que se están usando.


@subtitle{With-gensyms}

Sería un crimen hablar de captura de variable y @clref[cl:gensym] sin hablar de @code{with-gensyms}. Imagina que estamos intentando crear una macro que necesita crear unas 6 variables no internadas. El código podría ser algo parecido a esto:

@code-block[:lang "common-lisp"]{
(defmacro foo (&rest args)
  (let ((a (gensym "A"))
        (b (gensym "B"))
        (c (gensym "C"))
        (d (gensym "D"))
        (e (gensym "E"))
        (f (gensym "F")))
    ...))
}

Ya sólo con unos pocos @clref[cl:gensym] empezamos a tener un problema. Esto no es nada cómodo. Además, aunque no es obligatorio, es muy recomendable que el nombre que le pasamos a @clref[cl:gensym] sea igual al nombre de la variable por hacer más sencillo el debugueo. Eso significa que si queremos cambiar el nombre de una variable hay que cambiar también el string que se le pasa a @clref[cl:gensym].

¿Cómo podemos hacer más sencilla la tarea de llamar a todas estas llamadas a @clref[cl:gensym]?

Observa que hay mucha información redundante en la generación de los símbolos no internados. Por un lado, ya hemos dicho que el string pasado como argumento a @clref[cl:gensym] es igual al nombre de la variable. Así que podemos considerar el string como redundante. Por otro lado, estamos escribiendo 6 veces la palabra @code{gensym}. Nos gustaría crear algo para poder definir nuevos símbolos no internados usando @clref[cl:gensym] sin tener que escribir tantas veces la palabra @code{gensym} ni tener que duplicar el nombre de cada variable definida.

Es decir, podríamos tener algo como esto:

@code-block[:lang "common-lisp"]{
(with-gensyms (a b c d e f)
  ...
  )
}

@code{with-gensyms} va a ser una macro que defina las variables que pasemos como argumento. Estas variables almacenarán un símbolo no internado creado con @clref[cl:gensym] usando el nombre de la propia variable.

Empecemos a crear la macro. La cabecera de la macro sería la siguiente:

@code-block[:lang "common-lisp"]{
(defmacro with-gensyms ((&rest vars) &body body)
  ...)
}

Creo que es más o menos claro que la macro se tiene que expandir a la expresión @clref[let] que estábamos usando en la macro @code{foo}.

@code-block[:lang "common-lisp"]|{
(defmacro with-gensyms ((&rest vars) &body body)
  `(let ...
     ,@body))
}|

Y sólo nos faltaría la parte importante, la creación de los símbolos no internados. Para ello sólo tenemos que analizar qué información tenemos y qué queremos introducir dentro del @clref[let].

Nuestra variable @code{vars}, como ejemplo, puede tener las formas @code{(var1 var2)} o @code{(a b c d e f)}, por ejemplo. Es decir, @code{vars} es una lista. Y lo que necesitamos es transformar esta lista para que sea como @code{((var1 (gensym "VAR1")) (var2 (gensym "VAR2")))} o @code{((a (gensym "A")) (b (gensym "B")) (c (gensym "C")) ...)}.

Para esto podemos crear una función. Algo como esto podría valer:

@example{
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun syms-to-bindings (vars)
    (loop for var in vars
          collect `(,var (gensym ,(symbol-name var)))))

)
}

Observa que hemos usado @clref[eval-when]. Esto es imprescindible para asegurarnos que la función esté disponible incluso en tiempo de compilación. Si la macro se expande en tiempo de compilación y la función está definida de manera normal, entonces se lanzará un error.

Probemos a ver si funciona:

@example{
(syms-to-bindings '(a b c d e f))
}

¡Genial!

Ahora sólo queda usar la función en nuestra macro:

@example|{
(defmacro with-gensyms ((&rest vars) &body body)
  `(let ,(syms-to-bindings vars)
     ,@body))
}|

Fíjate que en este caso no hemos necesitado usar ningún símbolo no internado, pues todas las variables que se van a bindear están especificadas por los argumentos de la macro (las que contiene la lista @code{vars}).

Para terminar podemos redefinir nuestras macros @code{swap} y @code{repeat} usando @code{with-gensyms}.

@example|{
(defmacro swap (a b)
  (with-gensyms (aux)
    `(let ((,aux ,a))
       (setf ,a ,b)
       (setf ,b ,aux))))

(defmacro repeat (num &body exprs)
  (with-gensyms (aux)
    `(dotimes (,aux ,num)
       ,@exprs)))
}|

Ah, mucho mejor. Se queda el código más limpio y elegante. Si expandimos de nuevo el ejemplo veremos que seguimos distinguiendo los diferentes símbolos no internados:

@example{
(macroexpand-1 `(repeat 5
                  ,(macroexpand-1 '(swap a b))
                  ,(macroexpand-1 '(swap a c))
                  ,(macroexpand-1 '(swap b c))))
}

@subtitle{Recomendaciones finales}

Antes que crear tu propia macro @code{with-gensyms}, es recomendable utilizar alguna librería que ya la contenga. Mi recomendación es usar @link[:address "https://alexandria.common-lisp.dev/draft/alexandria.html"]{alexandría}. Además de @code{with-gensyms}, contiene @code{once-only} y funciones bastante útiles que podrían estar perfectamente en el estándar. Algunas de mis preferidas son @code{ensure-list} y @code{parse-body}, también muy útiles para escribir macros.

:D
