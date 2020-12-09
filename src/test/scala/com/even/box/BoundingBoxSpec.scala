package com.even.box

import com.even.box.BoundingBox.{ColRange, ContiguousStars, SpanningBox}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class BoundingBoxSpec extends AnyWordSpec{

  val testBox =
    """
  -----------
  ---------**
  ----**---**
  ----**----*
  ---------*-
  """

  val testBox2 =
    """
  -----------
  ----**---**
  ---**-----*
  --****---*-
  -----------
  ---***-----
  --****-----
  -----------
  """

  val testBox3 =
    """
**-------***
-*--**--***-
-----***--**
-------***--
    """

  val testBox4 =
    """
---------***
----**--***-
-----***--**
-------***--
    """

  val s1 = ContiguousStars(ColRange(2,3), 1)
  val s2 = ContiguousStars(ColRange(2,4), 2)
  val s3 = ContiguousStars(ColRange(2,5), 3)
  val s4 = ContiguousStars(ColRange(6,8), 4)  //Col Index Gap
  val s5 = ContiguousStars(ColRange(1,3), 10) //Row and Col Index Gap
  val s6 = ContiguousStars(ColRange(7,9), 11)
  val s7 = ContiguousStars(ColRange(8,9), 12)

  val groupedColumns= List(List(ContiguousStars(ColRange(2,3),1), ContiguousStars(ColRange(2,4),2), ContiguousStars(ColRange(2,5),3), ContiguousStars(ColRange(1,3),10)), List(ContiguousStars(ColRange(6,8),4), ContiguousStars(ColRange(7,9),11), ContiguousStars(ColRange(8,9),12)))
  val groupedColumnsAndRows = List(List(ContiguousStars(ColRange(2,3),1), ContiguousStars(ColRange(2,4),2), ContiguousStars(ColRange(2,5),3)), List(ContiguousStars(ColRange(7,9),11), ContiguousStars(ColRange(8,9),12)), List(ContiguousStars(ColRange(6,8),4)), List(ContiguousStars(ColRange(1,3),10)))

 "group contiguous" should {
   "group defined StarRow separated by None" in {
     BoundingBox.groupContiguous(List(None, Some(s1), Some(s2), None, None), Nil) shouldBe List(List(s1, s2))
     BoundingBox.groupContiguous(List(None, None, Some(s1), Some(s2), None, None, Some(s3), Some(s4)), Nil) shouldBe List(List(s1, s2), List(s3, s4))
   }
 }

  "find stars" should{
    "parse out '*' from input lines" in {
      BoundingBox.findStars(List( ('*', 0), ('*', 1), ('-', 2), ('*', 3), ('*', 4), ('-', 5)), 0, Nil) shouldBe List(ContiguousStars(ColRange(0, 1), 0), ContiguousStars(ColRange(3,4), 0))
    }
  }

  "join contiguous col" should{
    "join together col" in {
      BoundingBox.joinContiguousCol(10)(List(s1, s2, s3, s4, s5, s6, s7)) shouldBe groupedColumns
    }
  }

  "join contiguous row" should{
    "join together row" in {
      BoundingBox.joinContiguousRow(groupedColumns) shouldBe groupedColumnsAndRows
    }
  }

  "reduce subset rows" should{
    "remove smaller subsets that share rows" in {
      val s1 = ContiguousStars(ColRange(2,3), 1)
      val s2 = ContiguousStars(ColRange(2,4), 2)
      val s3 = ContiguousStars(ColRange(2,5), 3)
      val s4 = ContiguousStars(ColRange(6,8), 4)
      val s5 = ContiguousStars(ColRange(1,3), 10)

      BoundingBox.reduceSubSetRows(List(List(s1), List(s1, s2), List(s1, s2, s3))) shouldBe List(List(s1,s2,s3))
    }
  }

  "boxes overlapping" should{
    "return boolean to determine if overlapping" in {
      val box = SpanningBox(1, 3, 3, 6)
      val rightBox = SpanningBox(2, 4, 5, 7)
      val leftBox = SpanningBox(0, 4, 2, 9)
      val sb1 = SpanningBox(5,2,9,3)
      val sb2 = SpanningBox(8,0,11,2)
      val sb3 = SpanningBox(2,1,5,3)
      val sb4 = SpanningBox(2,5,5,6)
      val sb5 = SpanningBox(4, 4, 5, 7)

      BoundingBox.boxesOverlapping(box, rightBox) shouldBe true
      BoundingBox.boxesOverlapping(rightBox, box) shouldBe true
      BoundingBox.boxesOverlapping(leftBox, box) shouldBe true
      BoundingBox.boxesOverlapping(box, leftBox) shouldBe true
      BoundingBox.boxesOverlapping(sb1, sb2) shouldBe true
      BoundingBox.boxesOverlapping(sb2, sb1) shouldBe true
      BoundingBox.boxesOverlapping(sb1, sb2) shouldBe true
      BoundingBox.boxesOverlapping(sb3, sb4) shouldBe false
      BoundingBox.boxesOverlapping(sb4, sb3) shouldBe false
      BoundingBox.boxesOverlapping(sb1, sb5) shouldBe false
      BoundingBox.boxesOverlapping(sb5, sb1) shouldBe false
    }
  }
  "filter non overlapping" should{
    "filter overlapping boxes" in {
      val boxes = List(SpanningBox(4,1,5,1), SpanningBox(0,0,1,1), SpanningBox(8,0,11,2), SpanningBox(9,0,11,0), SpanningBox(5,2,9,3), SpanningBox(10,2,11,2), SpanningBox(8,1,10,1), SpanningBox(5,2,7,2), SpanningBox(4,1,7,2), SpanningBox(7,3,9,3))
      BoundingBox.filterNonOverlapping(boxes) shouldBe List(SpanningBox(0,0,1,1))
    }
    "filter nothing if no overlapping" in {
      val boxes = List(SpanningBox(2,1,5,3), SpanningBox(9,1,10,2), SpanningBox(2,5,5,6), SpanningBox(9,3,9,3))
      BoundingBox.filterNonOverlapping(boxes) shouldBe List(SpanningBox(2,1,5,3), SpanningBox(9,1,10,2), SpanningBox(2,5,5,6), SpanningBox(9,3,9,3))
    }
  }
  "min bounding box" should {
    "find min binding box for testbox" in {
      val lines = testBox.split("\n").map(_.trim).filterNot(_.isEmpty).toList
      BoundingBox.nonOverlappingMaxBindingBox(lines) shouldBe Some(SpanningBox(10, 2, 11, 4))
    }
    "find min binding box for testbox3" in {
      val lines = testBox3.split("\n").map(_.trim).filterNot(_.isEmpty).toList
      BoundingBox.nonOverlappingMaxBindingBox(lines) shouldBe Some(SpanningBox(1, 1, 2, 2))
    }
    "find min binding box for testbox2" in {
      val lines = testBox2.split("\n").map(_.trim).filterNot(_.isEmpty).toList
      BoundingBox.nonOverlappingMaxBindingBox(lines) shouldBe Some(SpanningBox(3, 2, 6, 4))
    }
    "find min no binding box when all overlapping" in {
      val lines = testBox4.split("\n").map(_.trim).filterNot(_.isEmpty).toList
      BoundingBox.nonOverlappingMaxBindingBox(lines) shouldBe None
    }
  }
}
