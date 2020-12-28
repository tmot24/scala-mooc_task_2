package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

import scala.annotation.tailrec
import scala.collection.immutable.LazyList.cons

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
    currentLine.foldLeft(List.empty[Int]) {
      (acc, currentInt) => acc match {
        case h :: count :: rest if currentInt == h => currentInt :: (count + 1) :: rest
        case _ => currentInt :: 1 :: acc
      }
    }.reverse
  }

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int] ] ...
   *
   */

  lazy val funSeq: LazyList[List[Int]] = LazyList.cons(hd = List(1), tl = funSeq.map(nextLine))
}