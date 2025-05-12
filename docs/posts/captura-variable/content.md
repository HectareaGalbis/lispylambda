
<a id="TITLE:LISPYLAMBDA:CAPTURA-VARIABLE"></a>
# Macros y la captura de variable

* [¿Qué es la captura de variable\?](/docs/posts/captura-variable/content.md#TITLE:LISPYLAMBDA:TAG51)
* [Evitando la captura de variable](/docs/posts/captura-variable/content.md#TITLE:LISPYLAMBDA:TAG52)
* [Gensym](/docs/posts/captura-variable/content.md#TITLE:LISPYLAMBDA:TAG53)
* [¿Cuándo debo usar [gensym](http://www.lispworks.com/reference/HyperSpec/Body/f_gensym.htm)\?](/docs/posts/captura-variable/content.md#TITLE:LISPYLAMBDA:TAG54)
* [With\-gensyms](/docs/posts/captura-variable/content.md#TITLE:LISPYLAMBDA:TAG55)
* [Recomendaciones finales](/docs/posts/captura-variable/content.md#TITLE:LISPYLAMBDA:TAG56)


Las macros de Common Lisp dan una libertad increible\. Pero esta libertad viene con un precio\. Si no tenemos cuidado\, podemos introducir bugs difíciles de detectar que nos darán dolor de cabeza durante unas buenas horas\. Estos bugs suelen estar ocasionados por la llamada **captura de variable**\.

<a id="TITLE:LISPYLAMBDA:TAG51"></a>
## ¿Qué es la captura de variable\?

Consideremos el ejemplo de crear una macro ```swap```\. Debe recibir dos argumentos e intercambiar sus valores\. Pensemos primero en el código al que debe expandirse\. Si queremos intercambiar el valor de dos variables lo haríamos de esta manera\:

`````common-lisp
(let ((a "a") (b "b"))

  (let ((aux a))
    (setf a b)
    (setf b aux))

  (format nil "a: ~a | b: ~a" a b))
`````
`````common-lisp
;; Returns
"a: b | b: a"
`````

Como se puede ver\, el código al que debe expandirse nuestra macro es\:

`````common-lisp
(let ((aux a))
  (setf a b)
  (setf b aux))
`````

La macro ```swap``` que estamos buscando podría ser perfectamente la siguiente\:

`````common-lisp
(defmacro swap (a b)
  `(let ((aux ,a))
     (setf ,a ,b)
     (setf ,b aux)))
`````
`````common-lisp
;; Returns
SWAP
`````

A primera vista\, parece que esté perfecta\. De hecho\, la mayoría de veces funcionará sin problemas\.

`````common-lisp
(let ((a "a") (b "b"))
  (swap a b)
  (format nil "a: ~a | b: ~a" a b))
`````
`````common-lisp
;; Returns
"a: b | b: a"
`````

Pero tarde o temprano\, los bugs acaban por manifestarse\.

`````common-lisp
(let ((a "a") (aux 5))
  (swap a aux)
  (format nil "a: ~a | aux: ~a" a aux))
`````
`````common-lisp
;; Returns
"a: a | aux: 5"
`````

Observa que ```swap``` no ha hecho absolutamente nada\. Las variables ```a``` y ```aux``` están intactas\. Evidentemente el ejemplo sugiere que el problema está al haber introducido la variable ```aux```\. Y casualmente es la misma variable que hemos usado en la definición de la macro ```swap```\. Para despejar las dudas\, vamos a expandir la anterior llamada de ```swap```\.

`````common-lisp
(macroexpand-1 '(swap a aux))
`````
`````common-lisp
;; Returns
(LET ((AUX A))
  (SETF A AUX)
  (SETF AUX AUX))
T
`````

Analicemos detenidamente el ejemplo con el código expandido\.

`````common-lisp
(let ((a "a") (aux 5))
  (let ((aux a))
    (setf a aux)
    (setf aux aux)))
`````

En un inicio tenemos que ```a``` vale ```"a"``` y que ```aux``` vale ```5```\. En el ```let``` interno se vuelve a asignar un nuevo valor para ```aux```\, en este caso el valor de la variable ```a```\, es decir\, ```"a"```\. Hay que tener en cuenta que aquí aún no se ha hecho ninguna asignación\. Las variables del ```let``` externo siguen teniendo los mismos valores\. Con el ```let``` interno se está _declarando_ una nueva variable ```aux```\. Se dice que el ```aux``` del ```let``` interno está ensombreciendo al ```aux``` del ```let``` externo\. Dentro del ```let``` interno tenemos entonces las variables ```a``` y ```aux``` que contienen el mismo valor ```"a"```\. Además\, hemos perdido el acceso al valor original de la variable ```aux``` del ```let``` externo\. Ya no tenemos acceso al valor ```5```\, por lo que nunca podremos asignárselo a la variable ```a```\. Por tanto\, a la variable ```a``` se le asigna el valor ```"a"``` que contiene el ```aux``` interno\. Y a la variable ```aux```\, que sigue siendo la variable del ```let``` interno se le asigna el valor ```"a"```\, es decir\, lo que ya tenía\.

En resumen\, a la variable ```a``` se le asigna el valor que ya tenía y a la variable ```aux``` no se le asigna nada porque es ensombrecida por un ```let``` interno\.

El problema de nuestra macro es que ha permitido que tanto el argumento como la variable auxiliar utilicen la misma variable que en este caso era la variable ```aux```\. Se ha producido una **colisión de nombres**\.

Es esta **colisión de nombres** lo que denominamos **captura de variable**\.


<a id="TITLE:LISPYLAMBDA:TAG52"></a>
## Evitando la captura de variable

Tras saber que una captura de variable es una colisión de nombres la pregunta es obvia\: ¿Cómo evitamos la colisión de nombres\? Y la respuesta parece obvia\: Necesitamos usar variables en nuestra macro que siempre vayan a ser diferentes a cualquier argumento que nos puedan pasar\.

Si estuviesemos hablando de las macros del lenguage C o C\+\+\, diríamos que basta con usar una variable con un nombre lo suficientemente raro\. Eso bajaría las problabilidades de colisión de nombres\, pero aún puede darse la posibilidad\. Por suerte estamos en Common Lisp\, un lenguaje infinitamente superior\. Y si el lenguaje nos permite hacer las cosas bien\, no deberíamos limitarnos a usar un nombre lo suficientemente raro\.

Hasta ahora hemos hablado de colisión de nombres pero recordemos que en Common Lisp usamos símbolos\. Así que lo buscamos realmente es evitar la colisión de símbolos\. Así que la pregunta que sigue realmente necesitamos hacer es\: ¿Qué símbolos son siempre diferentes a cualquier argumento que reciba nuestra macro\?

Para entender bien la respuesta\, pensemos en cómo se comparan los símbolos\. Recordemos que los símbolos son en realidad objetos\. En particular\, los símbolos contienen un nombre y pertenecen a un paquete\.

`````common-lisp
(symbol-name 'aux)
`````
`````common-lisp
;; Returns
"AUX"
`````

`````common-lisp
(symbol-package 'aux)
`````
`````common-lisp
;; Returns
#<PACKAGE "LISPYLAMBDA">
`````

En este caso el símbolo ```aux``` tiene de nombre ```"AUX"``` y pertenece al paquete ```"LISPYLAMBDA"```\.

A la hora de comparar si dos símbolos son iguales\, Common Lisp comprueba si son literalmente el mismo objeto\. La clave está en que cuando escribimos ```'aux```\, Common Lisp entiende que queremos el símbolo con nombre ```"AUX"``` y que pertenece\, en este caso\, al paquete actual \(```"LISPYLAMBDA"```\)\. Así que inmediatamente se va al paquete ```"LISPYLAMBDA"``` y busca algún símbolo cuyo nombre sea ```"AUX"```\. Si lo encuentra\, devuelve el símbolo\, o dicho de otra forma\, el objeto de tipo símbolo\.

Sabiendo esto\, podemos ver que a la hora de comparar dos símbolos siempre estarán involucrados tanto el nombre como el paquete de cada símbolo\. En particular\, dos símbolos serán iguales si tanto su nombre como su paquete son el mismo\. Esto nos da las dos siguientes opciones en nuestra búsqueda del símbolo perfecto para nuestra macro ```swap```\:

* Buscar un símbolo con algún nombre especial\.
* Buscar un símbolo con algún paquete especial\.


Suponiendo que los paquetes son iguales\, es claro que si un símbolo tiene un nombre en específico\, nuestra macro ```swap``` siempre podría recibir dicho símbolo con dicho nombre\. Por ejemplo\, si usamos un símbolo con nombre ```"__lcj2w78fh73x3  NQ93acÑ_wclnc   qwola"```\, siempre podríamos recibirlo como argumento\, aunque sea muy raro\. Y sí\, un símbolo con ese nombre puede existir\.

`````common-lisp
(symbol-name '__\l\c\j2\w78\f\h73\x3\ \ NQ93\a\cÑ_\w\c\l\n\c\ \ \ \q\w\o\l\a)
`````
`````common-lisp
;; Returns
"__lcj2w78fh73x3  NQ93acÑ_wclnc   qwola"
`````

Sólo nos queda entonces la segunda opción\. Necesitamos centrar nuestra búsqueda en los paquetes\.

Decíamos que cuando escribimos un símbolo\, Common Lisp busca en el paquete correspondiente un símbolo con el nombre del símbolo a buscar\. Pero\, ¿y si el símbolo no pertenece a ningún paquete\? De esta forma\, Common Lisp nunca podría encontrarlo a partir de un nombre\. Common Lisp busca siempre en el paquete correspondiente\, ¡pero el símbolo no está en ninguno\!

Recordemos que los símbolos que no pertenecen a ningún paquetes se denominan símbolos no internados y podemos crear estos símbolos de forma muy sencilla\.

Una de las formas más sencillas es usar el prefijo de paquete ```#:```\.

`````common-lisp
(symbol-package '#:hey)
`````
`````common-lisp
;; Returns
NIL
`````

Al ser un símbolo no internado\, su paquete es ```NIL```\. Es decir\, no pertenece a ningún paquete\.

Para estar seguros de que este es el tipo de símbolos que necesitamos podemos crear una macro que compruebe que el argumento sea igual al símbolo no internado que usemos\.

`````common-lisp
(defmacro arg-eq-p (sym)
  `(eq '#:aux ',sym))
`````
`````common-lisp
;; Returns
ARG-EQ-P
`````

Probemos\.

`````common-lisp
(arg-eq-p a)
`````
`````common-lisp
;; Returns
NIL
`````

`````common-lisp
(arg-eq-p aux)
`````
`````common-lisp
;; Returns
NIL
`````

`````common-lisp
(arg-eq-p #:aux)
`````
`````common-lisp
;; Returns
NIL
`````

Observa que en cada uno de los ejemplos se verifica que el argumento nunca es igual al símbolo no internado ```#:aux``` de la macro ```arg-eq-p```\. Y esto también ocurre en el último ejemplo\. De hecho\, no hacía falta crear ninguna macro para comprobar esto\:

`````common-lisp
(eq '#:aux '#:aux)
`````
`````common-lisp
;; Returns
NIL
`````

Aunque parezca que son el mismo símbolo por tener la misma representación gráfica\, no lo son en absoluto\. Este comportamiento está especificado en [la sección 2\.4\.8\.5 del hyperspec](https://www.lispworks.com/documentation/HyperSpec/Body/02_dhe.htm)\:

> \#\: introduces an uninterned symbol whose name is symbol\-name\.<br>
> Every time this syntax is encountered\, a distinct uninterned symbol is created\.

Cada vez que escribamos ```#:un-simbolo``` se creará un nuevo símbolo internado diferente\. Por eso en nuestro ejemplo se indica que son diferentes\, porque al escribir dos veces ```#:aux``` tenemos en total dos símbolos no internados con el nombre ```"AUX"```\.

Con esto ya podemos hacer nuestra macro\. Un primer intento podría ser el siguiente\.

`````common-lisp
(defmacro swap (a b)
  `(let ((#:aux ,a))
     (setf ,a ,b)
     (setf ,b #:aux)))
`````
`````common-lisp
;; Returns
SWAP
`````

¡Pero recuerda\! Cada vez que escribamos ```#:aux``` se va a crear un nuevo símbolo\. Por tanto los dos símbolos ```#:aux``` de nuestra macro son diferentes\. ¿Cómo solucionamos esto\? Basta guardar el símbolo no internado en una variable\.

`````common-lisp
(defmacro swap (a b)
  (let ((aux-sym '#:aux))
    `(let ((,aux-sym ,a))
       (setf ,a ,b)
       (setf ,b ,aux-sym))))
`````
`````common-lisp
;; Returns
SWAP
`````

Ahora si\. Observa que estamos guardando el símbolo no internado en la variable ```aux-sym```\. Seguidamente\, creamos el código que vamos a devolver\. Cada vez que queramos usar la variable no internada basta con obtenerla evaluando la variable ```aux-sym```\. De ahí que se esté usando la coma con ```aux-sym```\.

Si probamos ahora\, la macro ya debe funcionar perfectamente\:

`````common-lisp
(let ((a "a") (aux 5))
  (swap a aux)
  (format nil "a: ~s | aux: ~s" a aux))
`````
`````common-lisp
;; Returns
"a: 5 | aux: \"a\""
`````

Incluso podemos forzar el uso de una variable ```#:aux``` para ver que realmente funciona\:

`````common-lisp
(defmacro swap-with-aux ()
  (let ((aux-sym '#:aux)
        (a-sym '#:a))
    `(let ((,a-sym "a") (,aux-sym 5))
       (swap ,a-sym ,aux-sym)
       (format nil "a: ~s | aux: ~s" ,a-sym ,aux-sym))))
`````
`````common-lisp
;; Returns
SWAP-WITH-AUX
`````

Fíjate que también hemos creado un símbolo no internado ```#:a```\. Podría ocurrir que tengamos una variable global ```a``` en nuestro código\, así que más vale prevenir que curar\.

`````common-lisp
(swap-with-aux)
`````
`````common-lisp
;; Returns
"a: 5 | aux: \"a\""
`````


<a id="TITLE:LISPYLAMBDA:TAG53"></a>
## Gensym

En la práctica\, la macro se puede considerar perfecta\. Ya no fallará nunca\. Está libre de bugs\. Pero hay un pequeño detalle que nos puede jugar una mala pasada\. Estas macros son pequeñas\, pero en un proyecto real las macros pueden ser muy grandes\, por lo que siempre acabaremos recurriendo a algún sistema de debugueo\. En particular\, la herramienta más usada es [macroexpand\-1](http://www.lispworks.com/reference/HyperSpec/Body/f_mexp_.htm) o [macroexpand](http://www.lispworks.com/reference/HyperSpec/Body/f_mexp_.htm)\.

Probemos a expandir la macro ```swap-with-aux```\:

`````common-lisp
(macroexpand-1 '(swap-with-aux))
`````
`````common-lisp
;; Returns
(LET ((#:A "a") (#:AUX 5))
  (SWAP #:A #:AUX)
  (FORMAT NIL "a: ~s | aux: ~s" #:A #:AUX))
T
`````

Nos interesa también expandir la llamada a ```swap```\, pero no existe una función en el estándar de Common Lisp que nos permita hacer esto\. Por ello\, me voy a permitir el lujo de usar la librería [trivial\-macroexpand\-all](https://github.com/cbaggers/trivial-macroexpand-all)\.


`````common-lisp
(trivial-macroexpand-all:macroexpand-all '(swap-with-aux))
`````
`````common-lisp
;; Returns
(LET ((#:A "a") (#:AUX 5))
  (LET ((#:AUX #:A))
    (SETQ #:A #:AUX)
    (SETQ #:AUX #:AUX))
  (FORMAT NIL "a: ~s | aux: ~s" #:A #:AUX))
T
T
`````

Recordemos que ya hemos deducido que las macros son correctas\. Pero hay un claro problema aquí\. ¡No podemos distinguir qué ```#:aux``` es cuál\! Uno de los símbolos ```#:aux``` pertenece a la macro ```swap```\. Y el otro símbolo ```#:aux``` pertenece a la macro ```swap-with-aux```\. Al ser dos macros sencillas\, podemos acabar deduciendo cuál es cuál mirando las definiciones de cada macro\. Pero está claro que esto sería un problema muy gordo si usamos macros mucho más grandes\.

Recapitulemos qué tenemos y qué necesitamos ahora\. Hemos visto que necesitamos símbolos no internados para nuestras macros\. Pero ahora también queremos que sus nombres sean diferentes para poder diferenciarlos a la hora de debuguear\.

Por suerte\, los diseñadores de Common Lisp pensaron en todo y nos dieron la función [gensym](http://www.lispworks.com/reference/HyperSpec/Body/f_gensym.htm)\.

Primero\, lo importante\. Si nos fijamos en la documentación encontramos esta frase\:

> Creates and returns a fresh\, uninterned symbol\, \.\.\.

Bien\, crea un símbolo no internado\. Y lo segundo\, es que está hecho para asegurar que cada vez consigamos un nombre diferente\. Basta ver el siguiente ejemplo para entender cómo funciona\:

`````common-lisp
(loop for i from 0 below 10
      collect (gensym "AUX"))
`````
`````common-lisp
;; Returns
(#:AUX0 #:AUX1 #:AUX2 #:AUX3 #:AUX4 #:AUX5 #:AUX6 #:AUX7 #:AUX8 #:AUX9)
`````

Acabamos de crear una lista con 10 símbolos no internados\. Observa que cada símbolo tiene añadido un sufijo\. Este sufijo es un número que irá aumentando de 1 en 1 cada vez que se llame a [gensym](http://www.lispworks.com/reference/HyperSpec/Body/f_gensym.htm)\. En particular\, se aumenta en 1 la variable [\*gensym\-counter\*](http://www.lispworks.com/reference/HyperSpec/Body/v_gensym.htm)\.

Es tan simple el funcionamiento\, que nada te impide modificar la variable o indicar un prefijo para intentar que dos variables no internadas acaben con el mismo nombre\. Es decir\, el método no es perfecto\.

`````common-lisp
(list
  (let ((*gensym-counter* 999))
    (gensym "HEY"))
  (let ((*gensym-counter* 999))
    (gensym "HEY")))
`````
`````common-lisp
;; Returns
(#:HEY10 #:HEY11)
`````

Al ser estos símbolos no internados\, Common Lisp no tiene una forma directa de saber qué símbolos ha creado anteriormente\. Y aunque una opción posible sería ir guardándolos en algún contenedor\, con el tiempo este contenedor se haría enorme ocupando un espacio de memoria valioso\.

Como el programa va a funcionar perfectamente\, podemos al menos sacrificar que los nombres no siempre vayan a ser diferentes\.

Aunque ya te puedo asegurar que en prácticamente todo el tiempo que le dediques a debuguear macros \(y si no has modificado la variable [\*gensym\-counter\*](http://www.lispworks.com/reference/HyperSpec/Body/v_gensym.htm)\) nunca te vas a encontrar con el remoto caso de que dos símbolos no internados diferentes acaben con el mismo nombre\.

Dicho esto\, modifiquemos nuestras macros ```swap``` y ```swap-with-aux```

`````common-lisp
(defmacro swap (a b)
  (let ((aux-sym (gensym "AUX")))
    `(let ((,aux-sym ,a))
       (setf ,a ,b)
       (setf ,b ,aux-sym))))
`````
`````common-lisp
;; Returns
SWAP
`````

`````common-lisp
(defmacro swap-with-aux ()
  (let ((aux-sym (gensym "AUX"))
        (a-sym (gensym "A")))
    `(let ((,a-sym "a") (,aux-sym 5))
       (swap ,a-sym ,aux-sym)
       (format nil "a: ~s | aux: ~s" ,a-sym ,aux-sym))))
`````
`````common-lisp
;; Returns
SWAP-WITH-AUX
`````

Y por último\, veamos la expansión total de la macro ```swap-with-aux```\.

`````common-lisp
(trivial-macroexpand-all:macroexpand-all '(swap-with-aux))
`````
`````common-lisp
;; Returns
(LET ((#:A13 "a") (#:AUX12 5))
  (LET ((#:AUX14 #:A13))
    (SETQ #:A13 #:AUX12)
    (SETQ #:AUX12 #:AUX14))
  (FORMAT NIL "a: ~s | aux: ~s" #:A13 #:AUX12))
T
T
`````

Ahora sí\. Mucho mejor\. Obviamente no es el código más legible\, pero al menos podemos distinguir las diferentes variables que se están usando\.


<a id="TITLE:LISPYLAMBDA:TAG54"></a>
## ¿Cuándo debo usar [gensym](http://www.lispworks.com/reference/HyperSpec/Body/f_gensym.htm)\?

La regla de oro consiste en usar [gensym](http://www.lispworks.com/reference/HyperSpec/Body/f_gensym.htm) siempre que necesitemos alguna variable auxiliar como en el caso de ```swap```\.

Para la macro ```swap``` necesitábamos una variable auxiliar ```#:aux``` para poder realizar el intercambio de valores\. Por otro lado\, para la macro ```swap-with-aux``` necesitábamos dos variables auxiliares donde colocar los valores que queremos intercambiar\.

Aunque a veces no es tan obvio\, pues no siempre estas variables se definen con un [let](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm) o un [multiple\-value\-bind](http://www.lispworks.com/reference/HyperSpec/Body/m_multip.htm)\.

Supongamos que queremos una macro que nos permita repetir varias veces la ejecución de una o varias expresiones\. La manera más sencilla de hacer esto es usar la macro [dotimes](http://www.lispworks.com/reference/HyperSpec/Body/m_dotime.htm)\.

`````common-lisp
(dotimes (i 5)
  (princ "Hola")
  (terpri) ; Nueva linea
  )
`````
`````text
;; Output
Hola
Hola
Hola
Hola
Hola

`````
`````common-lisp
;; Returns
NIL
`````

Buscamos el mismo comportamiento sin tener que especificar una variable como ```i```\. Sólo queremos indicar el número y las expresiones\. Una opción sería esta\:

`````common-lisp
(defmacro repeat (num &body exprs)
  `(dotimes (i ,num)
     ,@exprs))
`````
`````common-lisp
;; Returns
REPEAT
`````

La forma de usarla es sencilla\:

`````common-lisp
(repeat 10
  (princ "Hola mundo")
  (terpri))
`````
`````text
;; Output
Hola mundo
Hola mundo
Hola mundo
Hola mundo
Hola mundo
Hola mundo
Hola mundo
Hola mundo
Hola mundo
Hola mundo

`````
`````common-lisp
;; Returns
NIL
`````

Pero claro\, internamente la macro [dotimes](http://www.lispworks.com/reference/HyperSpec/Body/m_dotime.htm) bindea la variable ```i``` con un valor del 0 al 9 para cada iteración del bucle\. Si utilizásemos una variable ```i``` el resultado podría no ser el esperado\:

`````common-lisp
(let ((i 5))
  (repeat 10
    (format t "i vale: ~a" i)
    (terpri)))
`````
`````text
;; Output
i vale: 0
i vale: 1
i vale: 2
i vale: 3
i vale: 4
i vale: 5
i vale: 6
i vale: 7
i vale: 8
i vale: 9

`````
`````common-lisp
;; Returns
NIL
`````

El resultado esperado es que siempre imprima ```i vale: 5```\, pero como [dotimes](http://www.lispworks.com/reference/HyperSpec/Body/m_dotime.htm) bindea nuevos valores a la variable ```i``` en cada iteración ocurre el desastre\.

La solución ya la sabemos\, usar [gensym](http://www.lispworks.com/reference/HyperSpec/Body/f_gensym.htm)\:

`````common-lisp
(defmacro repeat (num &body exprs)
  (let ((i (gensym "I")))
    `(dotimes (,i ,num)
       ,@exprs)))
`````
`````common-lisp
;; Returns
REPEAT
`````

Como ahora el simbolo usado es no internado\, todo funciona perfectamente\:

`````common-lisp
(let ((i 5))
  (repeat 10
    (format t "i vale: ~a" i)
    (terpri)))
`````
`````text
;; Output
i vale: 5
i vale: 5
i vale: 5
i vale: 5
i vale: 5
i vale: 5
i vale: 5
i vale: 5
i vale: 5
i vale: 5

`````
`````common-lisp
;; Returns
NIL
`````


<a id="TITLE:LISPYLAMBDA:TAG55"></a>
## With\-gensyms

Sería un crimen hablar de captura de variable y [gensym](http://www.lispworks.com/reference/HyperSpec/Body/f_gensym.htm) sin hablar de ```with-gensyms```\. Imagina que estamos intentando crear una macro que necesita crear unas 6 variables no internadas\. El código podría ser algo parecido a esto\:

`````common-lisp
(defmacro foo (&rest args)
  (let ((a (gensym "A"))
        (b (gensym "B"))
        (c (gensym "C"))
        (d (gensym "D"))
        (e (gensym "E"))
        (f (gensym "F")))
    ...))
`````

Ya sólo con unos pocos [gensym](http://www.lispworks.com/reference/HyperSpec/Body/f_gensym.htm) empezamos a tener un problema\. Esto no es nada cómodo\. Además\, aunque no es obligatorio\, es muy recomendable que el nombre que le pasamos a [gensym](http://www.lispworks.com/reference/HyperSpec/Body/f_gensym.htm) sea igual al nombre de la variable por hacer más sencillo el debugueo\. Eso significa que si queremos cambiar el nombre de una variable hay que cambiar también el string que se le pasa a [gensym](http://www.lispworks.com/reference/HyperSpec/Body/f_gensym.htm)\.

¿Cómo podemos hacer más sencilla la tarea de llamar a todas estas llamadas a [gensym](http://www.lispworks.com/reference/HyperSpec/Body/f_gensym.htm)\?

Observa que hay mucha información redundante en la generación de los símbolos no internados\. Por un lado\, ya hemos dicho que el string pasado como argumento a [gensym](http://www.lispworks.com/reference/HyperSpec/Body/f_gensym.htm) es igual al nombre de la variable\. Así que podemos considerar el string como redundante\. Por otro lado\, estamos escribiendo 6 veces la palabra ```gensym```\. Nos gustaría crear algo para poder definir nuevos símbolos no internados usando [gensym](http://www.lispworks.com/reference/HyperSpec/Body/f_gensym.htm) sin tener que escribir tantas veces la palabra ```gensym``` ni tener que duplicar el nombre de cada variable definida\.

Es decir\, podríamos tener algo como esto\:

`````common-lisp
(with-gensyms (a b c d e f)
  ...
  )
`````

```with-gensyms``` va a ser una macro que defina las variables que pasemos como argumento\. Estas variables almacenarán un símbolo no internado creado con [gensym](http://www.lispworks.com/reference/HyperSpec/Body/f_gensym.htm) usando el nombre de la propia variable\.

Empecemos a crear la macro\. La cabecera de la macro sería la siguiente\:

`````common-lisp
(defmacro with-gensyms ((&rest vars) &body body)
  ...)
`````

Creo que es más o menos claro que la macro se tiene que expandir a la expresión [let](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm) que estábamos usando en la macro ```foo```\.

`````common-lisp
(defmacro with-gensyms ((&rest vars) &body body)
  `(let ...
     ,@body))
`````

Y sólo nos faltaría la parte importante\, la creación de los símbolos no internados\. Para ello sólo tenemos que analizar qué información tenemos y qué queremos introducir dentro del [let](http://www.lispworks.com/reference/HyperSpec/Body/s_let_l.htm)\.

Nuestra variable ```vars```\, como ejemplo\, puede tener las formas ```(var1 var2)``` o ```(a b c d e f)```\, por ejemplo\. Es decir\, ```vars``` es una lista\. Y lo que necesitamos es transformar esta lista para que sea como ```((var1 (gensym "VAR1")) (var2 (gensym "VAR2")))``` o ```((a (gensym "A")) (b (gensym "B")) (c (gensym "C")) ...)```\.

Para esto podemos crear una función\. Algo como esto podría valer\:

`````common-lisp
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun syms-to-bindings (vars)
    (loop for var in vars
          collect `(,var (gensym ,(symbol-name var)))))

)
`````
`````common-lisp
;; Returns
SYMS-TO-BINDINGS
`````

Observa que hemos usado [eval\-when](http://www.lispworks.com/reference/HyperSpec/Body/s_eval_w.htm)\. Esto es imprescindible para asegurarnos que la función esté disponible incluso en tiempo de compilación\. Si la macro se expande en tiempo de compilación y la función está definida de manera normal\, entonces se lanzará un error\.

Probemos a ver si funciona\:

`````common-lisp
(syms-to-bindings '(a b c d e f))
`````
`````common-lisp
;; Returns
((A (GENSYM "A")) (B (GENSYM "B")) (C (GENSYM "C")) (D (GENSYM "D"))
 (E (GENSYM "E")) (F (GENSYM "F")))
`````

Genial\!

Ahora sólo queda usar la función en nuestra macro\:

`````common-lisp
(defmacro with-gensyms ((&rest vars) &body body)
  `(let ,(syms-to-bindings vars)
     ,@body))
`````
`````common-lisp
;; Returns
WITH-GENSYMS
`````

Fíjate que en este caso no hemos necesitado usar ningún símbolo no internado\, pues todas las variables que se van a bindear están especificadas por los argumentos de la macro \(las que contiene la lista ```vars```\)\.

Para terminar podemos redefinir nuestras macros ```swap``` y ```swap-with-aux``` usando ```with-gensyms```\.

`````common-lisp
(defmacro swap (a b)
  (with-gensyms (aux)
    `(let ((,aux ,a))
       (setf ,a ,b)
       (setf ,b ,aux))))

(defmacro swap-with-aux ()
  (with-gensyms (a aux)
    `(let ((,a "a") (,aux 5))
       (swap ,a ,aux)
       (format nil "a: ~s | aux: ~s" ,a ,aux))))
`````
`````common-lisp
;; Returns
SWAP-WITH-AUX
`````

Ah\, mucho mejor\. Se queda el código más limpio y elegante\. Si expandimos de nuevo la macro veremos que seguimos distinguiendo los diferentes símbolos no internados\:

`````common-lisp
(trivial-macroexpand-all:macroexpand-all '(swap-with-aux))
`````
`````common-lisp
;; Returns
(LET ((#:A16 "a") (#:AUX17 5))
  (LET ((#:AUX18 #:A16))
    (SETQ #:A16 #:AUX17)
    (SETQ #:AUX17 #:AUX18))
  (FORMAT NIL "a: ~s | aux: ~s" #:A16 #:AUX17))
T
T
`````


<a id="TITLE:LISPYLAMBDA:TAG56"></a>
## Recomendaciones finales

Antes que crear tu propia macro ```with-gensyms```\, es recomendable utilizar alguna librería que ya la contenga\. Mi recomendación es usar [alexandría](https://alexandria.common-lisp.dev/draft/alexandria.html)\. Además de ```with-gensyms```\, contiene ```once-only``` y funciones bastante útiles que podrían estar perfectamente en el estándar\. Algunas de mis preferidas son ```ensure-list``` y ```parse-body```\, también muy útiles para escribir macros\.

\:D