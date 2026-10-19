object parte_1 {
    //Parte ejercicio 3
    def transformar(numero: Int, operacion: Int => Int): Int = {
        operacion(numero)
    }

    //Parte del ejercicio 4
    def calcular(a: Double, b: Double, operacion: (Double, Double) => Double): Double = {
        operacion(a, b)
    }

    //Parte del ejercicio 5
    def crearMultiplicador(factor: Int): Int => Int = {
        (num: Int) => num*factor
    }

    //Parte del ejercicio 6
    def aplicarDescuento(precio: Double, porcentaje: Double): Double = {
        precio - precio*porcentaje
    }

    //Parte del ejercicio 7
    def elevar(exponente: Int)(base: Int): Int = {
        if exponente == 0 then 1
        else base*elevar(exponente -1)(base)
    }

    //Parte del ejercicio 9
    def sumar(desde: Int, hasta: Int, transformar: Int => Int): Int = {
        if desde == hasta then transformar(hasta)
        else transformar(desde) + sumar(desde + 1, hasta, transformar)
    }
    def producto(desde: Int, hasta: Int, transformar: Int => Int): Int = {
        if desde == hasta then transformar(hasta)
        else transformar(desde) * producto(desde + 1, hasta, transformar)
    }

    //Parte del ejercicio 10
    def transformarTexto(texto: String, transformacion: Char => Char): String = {
        if texto.length() == 1 then s"${transformacion(texto.head)}"
        else s"${transformacion(texto.head)}" + transformarTexto(texto.tail, transformacion)
    }
    def buscarCaracter(texto: String, condicion: Char => Boolean): Boolean = {
        if texto.isEmpty() then false
        else if condicion(texto.head) then true
        else buscarCaracter(texto.tail, condicion)
    }

    def main(args: Array[String]): Unit = {
        //Ejercicio 1
        val duplicar: Int => Int = (num: Int) => num * 2
        val cuadrado: Int => Int = (num: Int) => num * num
        val esPar: Int => Boolean = (num: Int) => num%2 == 0
        println(duplicar(15))
        println(cuadrado(9))
        println(esPar(12546))
        println()

        //Ejercicio 2
        val cubo: Int => Int = (num: Int) => num * num * num
        val promedio: (Double, Double) => Double = (a: Double, b: Double) => (a + b) / 2
        val mayor: (Int, Int) => Int = (a: Int, b: Int) => {if a > b then a else b}
        val esMultiploDeTres: Int => Boolean = (num: Int) => num % 3 == 0
        println(cubo(4))
        println(promedio(87, 94))
        println(mayor(58, 85))
        println(esMultiploDeTres(81))
        println()

        //Ejercicio 3
        println(transformar(15, duplicar))
        println(transformar(7, cubo))
        println(transformar(40, (num: Int) => num + 10))
        println(transformar(999, (num: Int) => - num))
        println()

        // Ejercicio 4
        println(calcular(75, 25, (a: Double, b: Double) => a + b))
        println(calcular(90, 40, (a: Double, b: Double) => a - b))
        println(calcular(7, 8,(a: Double, b: Double) => a * b))
        println(calcular(30, 5, (a: Double, b: Double) => a / b))
        println(calcular(87, 94, promedio))
        println()

        //Ejercicio 5
        val duplicar_2 = crearMultiplicador(2)
        val triplicar = crearMultiplicador(3)  
        val porDiez = crearMultiplicador(10)
        println(duplicar_2(24))
        println(triplicar(9))
        println(porDiez(5))
        println()

        //Ejercicio 6
        val descuento10: Double => Double = (precio: Double) => aplicarDescuento(precio, 0.1)
        val descuento20: Double => Double = (precio: Double) => aplicarDescuento(precio, 0.2)
        val descuento50: Double => Double = (precio: Double) => aplicarDescuento(precio, 0.5)
        println(descuento10(675.56))
        println(descuento20(239.50))
        println(descuento50(999.99))
        println()

        //Ejercicio 7
        val cuadrado_2 = elevar(2)
        val cubo_2 = elevar(3)
        val cuartaPotencia = elevar(4)
        println(cuadrado_2(5))
        println(cubo_2(4))
        println(cuartaPotencia(3))  
        println()

        //Ejercicio 8
        val limpiar: String => String = (text: String) => text.trim()
        val convertirMinusculas: String => String = (text: String) => text.toLowerCase()
        val longitud: String => Int = (text: String) => text.length()
        val esLongitudValida: Int => Boolean = (num: Int) => num >= 5
        val validarTexto: String => Boolean  = limpiar.andThen(convertirMinusculas).andThen(longitud).andThen(esLongitudValida)
        println(limpiar(" Soy tan Humano como Cualquiera "))
        println(convertirMinusculas(" Soy tan Humano como Cualquiera "))
        println(longitud(" Soy tan Humano como Cualquiera "))
        println(validarTexto(" Soy tan Humano como Cualquiera "))
        println()


        //Ejercicio 9
        println(sumar(1, 4, a => a))
        println(sumar(1, 4, a => a*a))
        println(sumar(1, 4, a => a*a*a))
        println(producto(1, 4, a => a))
        println(producto(1, 4, a => a*a))
        println()

        //Ejercicio 10
        println(transformarTexto("papas fritas", c => c.toUpper))
        println(buscarCaracter("Que somos cuando somos nada?", c => c.equals('?')))
    }
}