class Pelicula(val titulo: String, val duracionMinutos: Int, val clasificacion: String)

object Pelicula {
    def apply( titulo: String, duracionMinutos: Int, clasificacion: String): Pelicula = {
        new Pelicula(titulo, if duracionMinutos <= 0 then 90 else duracionMinutos, clasificacion)
    }
}

class FuncionCine(val pelicula: Pelicula, val sala: Int, val hora: String, val precioEntrada: Double, val asientosDisponibles: Int){
    def puedeReservar(cantidad: Int): Boolean = {
        asientosDisponibles - cantidad >= 0
    }
    def costoReserva(cantidad: Int): Double = {
        precioEntrada * cantidad
    }

    def reservar(cantidad: Int): FuncionCine = {
        if puedeReservar(cantidad) then new FuncionCine(pelicula, sala, hora, precioEntrada, asientosDisponibles - cantidad)
        else new FuncionCine(pelicula, sala, hora, precioEntrada, asientosDisponibles) 
    }

    def descripcion: String = {
        s"Pelicula: ${pelicula.titulo}, sala: $sala, horario: $hora, precio de entrada: $precioEntrada Bs, asientos disponibles: $asientosDisponibles" 
    }
}

object FuncionCine {
    def apply(pelicula: Pelicula, sala: Int, hora: String, precioEntrada: Double, asientosDisponibles: Int): FuncionCine = {
        new FuncionCine(pelicula, if sala <= 0 then 1 else sala, hora, if precioEntrada > 0 then precioEntrada else 0.0, if asientosDisponibles >= 0 then asientosDisponibles else 0 )
    }
    def funcionEstandar(pelicula: Pelicula, sala: Int, hora: String): FuncionCine = {
        new FuncionCine(pelicula, sala, hora, 30, 50)
    }
}

object main_parte2 {
    def main(args: Array[String]): Unit = {
        //Creando una pelicula y consultando por entradas
        val pelicula1 = Pelicula("La Sociedad de los Poetas Muertos", 128, "PG-13")
        val funcion1 = FuncionCine(pelicula1, 1, "9:00", 45.0, 50)
        println(funcion1.descripcion)
        println(funcion1.puedeReservar(4))
        println(funcion1.costoReserva(4))

        //realizando una reserva 
        val funcion1_2 = funcion1.reservar(4)
        println(funcion1_2.descripcion)
        println()

        //Creando una segunda pelicula
        val pelicula2 = Pelicula("Todo en todas partes al mismo tiempo", 139, "R")
        val funcion2 = FuncionCine.funcionEstandar(pelicula2, 2, "13:00")
        println(funcion2.descripcion)
        //Si no las vio, vealas ;) son muy buenas y la entrada no esta tan cara
    }
}