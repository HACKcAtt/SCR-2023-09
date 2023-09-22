package module1
import java.util.UUID
import scala.annotation.tailrec
import java.time.Instant
import scala.collection.immutable.Stack
import scala.collection.mutable.ListBuffer
import scala.language.{higherKinds, postfixOps}



/**
 * referential transparency
 */

// recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }

  def factRec(n: Int): Int =
    if(n <= 0) 1 else n * factRec(n - 1)

  def factRecTail(n: Int): Int = {

    @tailrec
    def loop(n: Int, accum: Int): Int =
      if( n <= 1) accum
      else loop(n - 1, n * accum)

    loop(n, 1)
  }

  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */

  // Imperative Fibonacci.
  def fibonacciImperative(n: Int): Int = {
    if (n <= 1) {
      print(n + ". Final result: " + n + ".")
      n
    }
    else {
      var a = 0
      var b = 1
      for (i <- 2 to n) {
        val sum = a + b
        if (i < n) print(sum + ", ") else print(sum + ". Final result: " + sum + ".")
        a = b
        b = sum
      }
      b
    }
  }

  // Recursive Fibonacci.
  def fibonacciRecursive(n: Int): Int = {
    if (n <= 1) n else fibonacciRecursive(n - 1) + fibonacciRecursive(n - 2)
  }

  // Tail recursive Fibonacci.
  def fibonacciTailRecursive(n: Int): Int = {
    @tailrec
    def fibonacciTail(n: Int, a: Int, b: Int): Int = {
      if (n == 0) a else fibonacciTail(n - 1, b, a + b)
    }

    return fibonacciTail(n, 0, 1)
  }


}

object hof{


  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result: B = f(a)
    val end = System.currentTimeMillis()
    println(end - start)
    result
  }

  def doomy(str: String) = {
    Thread.sleep(1000)
    println(str)
  }



  // изменение поведения ф-ции

  val arr = Array(1, 2, 3, 4, 5)

  def isOdd(i: Int): Boolean = i % 2 > 0

  def not[A](f: A => Boolean): A => Boolean = a => !f(a)

  def isEven: Int => Boolean = not(isOdd)


  // (A, B, C) => D   curring A => B => C => D
  // изменение самой функции

  def curried[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = curried(f)(a)

  def sum(x: Int, y: Int): Int  = x + y

  val s0: Int => Int => Int = curried(sum)

  val s1: Function1[Int, Int] = s0(2)

  val s2: Int = s1(3) // 5

  val p0: Int => Int = partial(2, sum)

  p0(3) // 5

  trait Consumer{
    def subscribe(topic: String): Stream[Record]
  }

  case class Record(value: String)

  case class Request()

  object Request {
    def parse(str: String): Request = ???
  }

  /**
   *
   * (Опционально) Реализовать ф-цию, которая будет читать записи Request из топика,
   * и сохранять их в базу
   */
  def createRequestSubscription() = ???
}

/**
 *  Реализуем тип Option
 */

object opt {

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутсвие результата
   */

  sealed trait Option[+T]{

    def isEmpty: Boolean =  this match {
      case Some(v) => false
      case None => true
    }

    def get: T = this match {
      case Some(v) => v
      case None => throw new Exception("None get")
    }

    def map[B](f: T => B): Option[B] = flatMap(v => Some(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Some(v) => f(v)
      case None => None
    }

    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */
    def filter(incoming: T => Boolean): Option[T] = this match {
      case Some(v) if (incoming(v)) => this
      case _ => None
    }

    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[TT](optionTwo: Option[TT]): Option[(T, TT)] = (this, optionTwo) match {
      case (Some(v), Some(vOptionTwo)) => Some((v, vOptionTwo))
      case _ => None
    }

    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny(): Unit = this match {
      case Some(v) => print(v)
      case None =>
    }
  }
  case class Some[T](v: T) extends Option[T]
  case object None extends Option[Nothing]

  object Option{
  }

  // Animal -> Dog
  // Covariant + отношения переносятся на контейнер
  // Contravariant - отношения переносятся на контейнер наоборот
  // Invariant - нет отношений
}

trait Animal
case object Dog
case object Cat

object list {
  /**
   *
   * Реализовать односвязанный иммутабельный список List
   * Список имеет два случая:
   * Nil - пустой список
   * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
   */

  Seq
  sealed trait List[+T] {
    /**
     * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
     *
     */

    def ::[TT >: T](head: TT): List[TT] = new ::(head, this)

    /**
     * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
     *
     */
    def mkString(delimiter: String): String = this match {
      case ::(head, Nil) => s"$head"
      case ::(head, tail) => s"$head$delimiter" + tail.mkString(delimiter)
      case Nil => ""
    }

    /**
     * Конструктор, позволяющий создать список из N - го числа аргументов
     * Для этого можно воспользоваться *
     *
     * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
     * def printArgs(args: Int*) = args.foreach(println(_))
     */

    /**
     *
     * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
     */
    def reverse: List[T] = {
      @tailrec
      def reverseTailRecursion(input: List[T], accum: List[T] = List[T]()): List[T] = {
        input match {
          case List.Nil => accum
          case List.::(head, tail) => reverseTailRecursion(tail, head :: accum)
        }
      }

      reverseTailRecursion(this)
    }

    /**
     *
     * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
     */
    def map[TT](f: T => TT): List[TT] = {
      @tailrec
      def mapTailRecursion(thisList: List[T], accum: List[TT]): List[TT] = thisList match {
        case list.::(head, tail) => mapTailRecursion(tail, list.::(f(head), accum));
        case list.Nil => accum.reverse
      }

      mapTailRecursion(this, List[TT]());
    }


    /**
     *
     * Реализовать метод filter для списка который будет фильтровать список по некому условию
     */
    def filter(f: T => Boolean): List[T] = {
      @tailrec
      def filterTailRecursion(currentList: List[T], acc: List[T]): List[T] = currentList match {
        case list.::(head, tail) if (f(head)) => filterTailRecursion(tail, list.::(head, acc));
        case list.::(_, tail) => filterTailRecursion(tail, acc);
        case list.Nil => acc
      }

      filterTailRecursion(this, List[T]())
        .reverse;
    }

     def :::[TT >: T](that: List[TT]): List[TT] = {
      @tailrec
      def tailRecursion(l1: List[TT], l2: List[TT], acc: List[TT]): List[TT] = l2 match {
        case ::(head, tail) => tailRecursion(l1, tail, acc.::(head))
        case Nil => l1 match {
          case ::(head, tail) => tailRecursion(tail, Nil, acc.::(head))
          case Nil => acc
        }
      }
       tailRecursion(this, that, Nil).reverse
    }




    def map[B](f: T => B): List[B] = ???

    def flatMap[B](f: T => List[B]): List[B] = this match {
      case ::(head, tail) => f(head) ::: tail.flatMap(f)
      case Nil => Nil
    }

    @tailrec
    final def foldLeft[B](acc: B)(op: (B, T) => B): B = this match {
      case ::(head, tail) => tail.foldLeft(op(acc, head))(op)
      case Nil => acc
    }

    def foldLeft2 = ???

    def take(n: Int): List[T] = {
      val r = this.foldLeft((0, List[T]())){ case ((i, acc), el) =>
        if( i == n) (i, acc)
        else (i + 1, acc.::(el))
      }
      r._2
    }

    def drop(n: Int): List[T] = ???

  }

  val l1: List[Int] = ???
  val l2: List[Int] = ???


  val l3: List[Int] = for{
    e1 <- l1
    e2 <- l2
  } yield e1 + e2

  val l4: List[Int] = l1.flatMap(e1 => l2.map(e2 => e1 + e2))


  val o1: Option[Int] = ???
  val o2: Option[Int] = ???

  val o3: Option[Int] = for{
    e1 <- o1
    e2 <- o2
  } yield e1 + e2






  case class ::[A](head: A, tail: List[A]) extends List[A]
  case object Nil extends List[Nothing]


  object List{
    // Constructor.
    case object Nil extends List[Nothing]

    case class ::[T](head: T, tail: List[T]) extends List[T]

    val Cons: ::.type = ::

    // Пример создания экземпляра с помощью конструктора apply
    def apply[T](v: T*): List[T] = if(v.isEmpty) Nil
    else ::(v.head, apply(v.tail:_*))
  }

  /**
   *
   * Написать функцию incList котрая будет принимать список Int и возвращать список,
   * где каждый элемент будет увеличен на 1
   */
  def incList(incomingInts: list.List[Int]): list.List[Int] = incomingInts.map(_ + 1);


  /**
   *
   * Написать функцию shoutString котрая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */
  def shoutString(stringsList: list.List[String]): list.List[String] = stringsList.map("!".concat);
}