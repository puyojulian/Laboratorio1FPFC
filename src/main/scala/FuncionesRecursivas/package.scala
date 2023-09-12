package object FuncionesRecursivas {
  // 1.1. Calcular el tamaño de una lista con un proceso iterativo
  def tamI(lista: List[Int]): Int = {
    def contItems(l: List[Int], cont: Int): Int = {
      if (l.isEmpty) cont else contItems(l.tail, cont + 1)
    }
    contItems(lista, 0)
  }

  // 1.2. Dividiento una lista en dos sublistas a partir de un pivote.
  def menoresQue(lista:List[Int], pivote:Int): List[Int] = {
    def agregarMenoresQue(l: List[Int]): List[Int] = {
      if (l.isEmpty)
        List()
      else
        if (l.head < pivote) {
          l.head::agregarMenoresQue(l.tail)
        }
        else
          agregarMenoresQue(l.tail)
    }
    agregarMenoresQue(lista)
  }

  def noMenoresQue(lista: List[Int], pivote: Int): List[Int] = {
    def agregarMayoresQue(l: List[Int]): List[Int] = {
      if (l.isEmpty)
        List()
      else if (l.head > pivote) {
        l.head :: agregarMayoresQue(l.tail)
      }
      else
        agregarMayoresQue(l.tail)
    }
    agregarMayoresQue(lista)
  }

  // 1.3. Calculando el k-esimo elemento de una lista
  def k_elem(l: List[Int], k: Int): Int = {
    val c: Int = k
    def k_elemCons(l: List[Int], c: Int, k: Int): Int = {
      if (!l.isEmpty) {
        val tamMenores: Int = tamI(menoresQue(l, c))
        if (tamMenores == k) c - 1: Int else k_elemCons(l, c + 1, k)
      }
      else {
        -1
      }
    }
    k_elemCons(l, c, k)
  }

  // 1.4. Ordenando una lista
  def ordenar(lista: List[Int]) = {
    def agregarDeMenorAMayor(l: List[Int]): List[Int] = {
      if (l.isEmpty)
        List()
      else if (menoresQue(l, l.head).isEmpty)
        l.head::agregarDeMenorAMayor(noMenoresQue(lista,l.head))
      else
        agregarDeMenorAMayor(l.tail)
    }
    agregarDeMenorAMayor(lista)
  }
}
