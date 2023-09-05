package object FuncionesRecursivas {
  // Ejemplo Profesor
  def tamRRec(lista:List[Int]): Int = { // Referencia Recursiva
    if (lista.isEmpty) 0
    else 1 + tamRRec(lista.tail)
  }

  // 1.1. Calcular el tamaño de una lista con un proceso iterativo
  def tamRIter(lista: List[Int]): Int = {
    def contItemsR(l: List[Int]) = 1 + tamRIter(l.tail)
    if (lista.isEmpty) 0 else contItemsR(lista)
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
  def k_elem(lista: List[Int], k: Int): Int = {
    def encontrarValorK(l: List[Int]): Int = {
      if (tamRIter(menoresQue(l,l.head)) == (k - 1))
        l.head
      else {
        if(l.tail.isEmpty)
          l.head
        else
          encontrarValorK(l.tail)
      }
    }
    encontrarValorK(lista)
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
