package com.even.box

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object BoundingBox extends App{
  case class ColRange(startIndex: Int, lastIndex: Int)
  case class ContiguousStars(range: ColRange, rowIndex: Int)
  case class SpanningBox(xmin: Int, ymin: Int, xmax: Int, ymax: Int)

  val text = Iterator.continually(readLine()).takeWhile(_.nonEmpty)
  scala.util.Try{ nonOverlappingMaxBindingBox(text.toList).fold(println("No non overlapping box"))(bb => println(s"(${bb.xmin},${bb.ymin})(${bb.xmax},${bb.ymax})")) }.recover{
    case t: Throwable => println(s"The input could not be handled.  Every line must have the same length and be either a '-' or '*'")
  }

  def nonOverlappingMaxBindingBox(lines: List[String]) = {
    val contiguousShapes = lineToStarRows _ andThen joinContiguousCol(lines.head.length) andThen joinContiguousRow apply(lines)
    val boxes = contiguousShapes.map(createBox)
    filterNonOverlapping(boxes).maxByOption(boxArea).map{ sb => SpanningBox(sb.xmin+1, sb.ymin+1, sb.xmax+1, sb.ymax+1) }
  }

  def lineToStarRows(lines: List[String]) =
    lines.zipWithIndex.flatMap{ case (row, lineIndex) => findStars(row.toList.zipWithIndex, lineIndex, Nil) }

  /**
   * Remove Lists that are subsets of other lists of ContiguousStars
   * i.e. List(List(1,2), List(List(1,2,3)) becomes List(1,2,3)
   *
   * @param rows
   * @return
   */
  def reduceSubSetRows(rows: List[List[ContiguousStars]]) = {
    rows.sortBy(_.length).reverse.foldLeft(List[List[ContiguousStars]]()){
      case (accSet, codes) if accSet.exists(s => codes.forall(s.contains)) => accSet
      case (accSet, codes) => accSet :+ codes
    }
  }

  /**
   * Group ContiguousStars elements into a list when they overlap column index values
   *
   * @param colMax
   * @param rows
   * @return
   */
  def joinContiguousCol(colMax: Int)(rows: List[ContiguousStars]) =
      reduceSubSetRows(for(i <- (0 until colMax).toList) yield rows.filter(r => r.range.startIndex <= i && r.range.lastIndex >= i))

  /**
   * Group together collections of ContiguousStars already grouped by column into collections that have contiguous row and column
   *
   * @param rows
   * @return
   */
  def joinContiguousRow(rows: List[List[ContiguousStars]]) = {
    reduceSubSetRows(rows.flatMap{ rs =>
      val indexMap = rs.map(r => r.rowIndex -> r).toMap
      val inOrder = for(i <- rs.map(_.rowIndex).min to rs.map(_.rowIndex).max) yield indexMap.get(i)
      groupContiguous(inOrder.toList, Nil)
    })
  }

  def boxArea(sb: SpanningBox) = (sb.xmax - sb.xmin) * (sb.ymax - sb.ymin)

  def boxesOverlapping(sb1: SpanningBox, sb2: SpanningBox) = {
    val xcheck = if(sb1.xmin <= sb2.xmin) sb2.xmin <= sb1.xmax else sb1.xmin <= sb2.xmax
    val ycheck = if(sb1.ymin <= sb2.ymin) sb2.ymin <= sb1.ymax else sb1.ymin <= sb2.ymax
    xcheck && ycheck
  }
  def filterNonOverlapping(boxes: List[SpanningBox]) = {
    boxes.foldLeft(List[SpanningBox]()){
      case (acc, box) if boxes.filterNot(_ == box).exists(p => boxesOverlapping(p, box)) => acc
      case (acc, b2) => acc:+b2
    }
  }

  def createBox(lines: List[ContiguousStars]) = {
      val xmin = lines.map(_.range.startIndex).min
      val xmax = lines.map(_.range.lastIndex).max
      val ymin = lines.map(_.rowIndex).min
      val ymax = lines.map(_.rowIndex).max
    SpanningBox(xmin, ymin, xmax, ymax)
  }

  /**
   * Group together rows with defined stars that are next to each other
   * Some(StarRow) represents a row with stars, None represents a row with no stars
   * Gaps Some(StarRow) -> (None) -> Some(StarRow) represent the end of a contiguous collection of rows
   *
   * @param elements
   * @param acc
   * @return
   */
  @tailrec
  def groupContiguous(elements: List[Option[ContiguousStars]], acc: List[List[ContiguousStars]]): List[List[ContiguousStars]] = {
    elements match {
      case Nil => acc
      case Some(_)::_=>
        val defined = elements.takeWhile(_.isDefined)
        groupContiguous(elements.drop(defined.size), acc:+defined.flatten)
      case None::_ =>
        val undefined = elements.takeWhile(_.isEmpty)
        groupContiguous(elements.drop(undefined.size), acc)
    }
  }

  /**
   * Parse each line and separate contiguous ''*'' characters from ''-'' characters
   *
   * @param line
   * @param lineIndex
   * @param acc
   * @return
   */
  @tailrec
  def findStars(line: List[(Char, Int)], lineIndex: Int, acc: List[ContiguousStars]): List[ContiguousStars] = {
    line match {
      case Nil => acc
      case ('*', _)::_ =>
        val stars = line.takeWhile(_._1 == '*').map{ case (_, index) => index }
        findStars(line.drop(stars.size), lineIndex, acc:+ ContiguousStars(ColRange(stars.head, stars.last), lineIndex))
      case (c, _)::_ =>
        findStars(line.drop(line.takeWhile(_._1 == c).size), lineIndex, acc)
    }
  }
}
