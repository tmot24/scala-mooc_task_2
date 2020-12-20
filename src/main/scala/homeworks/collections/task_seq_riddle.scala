package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

import scala.annotation.tailrec

object task_seq_riddle extends App {

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */

  def nextLine(currentLine: List[Int]): List[Int] = {
    @tailrec
    def nextLine2(currentLine: List[Int], acc: List[List[Int]] = Nil): List[Int] = {
      currentLine.headOption match {
        case None =>
          val list = acc.reverse
          val size = list.map(_.size)
          val zip = size.zip(list.flatMap(_.distinct))
          val result = for {
            t <- zip
          } yield List(t._1, t._2)
          result.flatten
        case Some(value) =>
          val firstValues = currentLine.takeWhile(_ == value)
          val restValues = currentLine.dropWhile(_ == value)
          nextLine2(restValues, firstValues :: acc)
      }
    }
    nextLine2(currentLine)
  }
  println(nextLine(List(1, 1, 1, 2, 2, 1)))

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */


//  val funSeq: LazyList[List[Int]] = ???
//    task"Реализуйте ленивый список, который генерирует данную последовательность"()
}