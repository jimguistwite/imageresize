package com.jgui.imageresize

import java.awt.image.BufferedImage

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec

object Direction extends Enumeration {
  type Direction = Value
  val Vertical, Horizontal, Undefined = Value
}

class ImageResizer(input: java.awt.image.BufferedImage) {
  final val EDGE_ENERGY: Double = 195075.0d

  import Direction._

  /**
   * Extend array of doubles with a function to return the index of the minimum value.
   */
  class MinArray[T <: Double](at: Array[T]) {
    def minIndex() = {
      var rv = 0
      var min = Double.MaxValue
      for (i <- 0 until at.length) {
        if (at(i) < min) {
          rv = i
          min = at(i)
        }
      }
      rv
    }
  }
  implicit def array2minarray[T <: Double](at: Array[T]) = new MinArray(at)

  type VerticalSeam = Array[Int]
  type HorizontalSeam = Array[Int]

  case class EnergizedPixel(var x: Int, var y: Int, var rgb: Int, var energy: Double, var minNodeTo: Int)

  class ResizingImage(source: java.awt.image.BufferedImage) {

    var width = source.getWidth()
    var height = source.getHeight()
    var horizontalDirt: Option[Array[(Int, Range)]] = None
    var verticalDirt: Option[Array[(Int, Range)]] = None
    var cascadedDirtPoint = (0, 0)
    var cascadedDirtDirection = Direction.Undefined

    val pixels = ArrayBuffer.tabulate(source.getWidth(), source.getHeight())((x, y) => {
      EnergizedPixel(x, y, source.getRGB(x, y), 0.0, 0)
    })

    // assign energy
    foreachPixel((p, x, y) => p.energy = energy(this, p))

    def row(i: Int): Seq[EnergizedPixel] = {
      val rv = ArrayBuffer[EnergizedPixel]()

      for (col <- 0 until width) rv += pixels(0)(i)

      rv.toSeq
    }

    def foreach(f: (EnergizedPixel) => Unit) {
      for (y <- 0 until height) {
        for (x <- 0 until width) {
          f(pixels(x)(y))
        }
      }
    }

    def foreachPixel(f: (EnergizedPixel, Int, Int) => Unit) {
      for (y <- 0 until height) {
        for (x <- 0 until width) {
          f(pixels(x)(y), x, y)
        }
      }
    }

    def foreachColumnOrderedWithIndices(f: (EnergizedPixel, Int, Int) => Unit) {
      for (x <- 0 until width) {
        for (y <- 0 until height) {
          f(pixels(x)(y), x, y)
        }
      }
    }

    def getUpdatedImage(): java.awt.image.BufferedImage = {
      val out = new BufferedImage(image.width, image.height, BufferedImage.TYPE_INT_RGB)
      image.foreachPixel((p, x, y) => {
        out.setRGB(x, y, p.rgb)
      })
      out
    }

  }

  val image = new ResizingImage(input)

  def getUpdatedImage(): java.awt.image.BufferedImage = {
    image.getUpdatedImage()
  }

  def energy(img: ResizingImage, p: EnergizedPixel): Double = {
    val x = p.x
    val y = p.y
    if ((x == 0) || (x == img.width - 1) || (y == 0)
      || (y == img.height - 1)) {
      EDGE_ENERGY
    } else {
      val left = img.pixels(x - 1)(y).rgb
      val right = img.pixels(x + 1)(y).rgb
      val up = img.pixels(x)(y - 1).rgb
      val down = img.pixels(x)(y + 1).rgb

      val hR = ((left >> 16) & 0xFF) - ((right >> 16) & 0xFF)
      val hG = ((left >> 8) & 0xFF) - ((right >> 8) & 0xFF)
      val hB = ((left >> 0) & 0xFF) - ((right >> 0) & 0xFF)

      val vR = ((up >> 16) & 0xFF) - ((down >> 16) & 0xFF)
      val vG = ((up >> 8) & 0xFF) - ((down >> 8) & 0xFF)
      val vB = ((up >> 0) & 0xFF) - ((down >> 0) & 0xFF)

      hR * hR + hG * hG + hB * hB + vR * vR + vG * vG + vB * vB
    }
  }

  def neighbors(image: ResizingImage, in: EnergizedPixel, dir: Direction): Array[EnergizedPixel] = {
    dir match {
      case Vertical => {
        if (in.y == 0) {
          Array(in, image.pixels(in.x)(in.y + 1))
        } else if (in.y == image.height) {
          Array(image.pixels(in.x)(in.y - 1), in)
        } else {
          Array(image.pixels(in.x)(in.y - 1), in, image.pixels(in.x)(in.y + 1))
        }
      }
      case Horizontal => {
        if (in.x == 0) {
          Array(in, image.pixels(in.x + 1)(in.y))
        } else if (in.x == image.width) {
          Array(image.pixels(in.x - 1)(in.y), in)
        } else {
          Array(image.pixels(in.x - 1)(in.y), in, image.pixels(in.x + 1)(in.y))
        }
      }
      case _ => {
        Array(in)
      }
    }
  }

  /**
   * Return a sequence of indices for vertical seam
   *
   * @return sequence of indices for vertical seam
   */
  def findVerticalSeam(): VerticalSeam = {

    sweepDirt()

    val minEnergy = Array.ofDim[Double](2, image.width)

    var current = 0
    var previous = 1

    // cascade the energy values through the image
    image.foreachPixel((p, x, y) => {
      if (y != 0) {
        p.minNodeTo = x
        minEnergy(current)(x) = minEnergy(previous)(x)

        if ((x != 0) && (minEnergy(previous)(x - 1) < minEnergy(current)(x))) {
          p.minNodeTo = x - 1
          minEnergy(current)(x) = minEnergy(previous)(x - 1)
        }
        if ((x != (image.width - 1)) && (minEnergy(previous)(x + 1) < minEnergy(current)(x))) {
          p.minNodeTo = x + 1
          minEnergy(current)(x) = minEnergy(previous)(x + 1)
        }
      }
      // add this guy's energy to whatever we have so far while traversing.
      minEnergy(current)(x) += p.energy

      if (x == image.width - 1) {
        current ^= 1
        previous ^= 1
      }
    })

    // these alternates add 2-3 ms 
    //val foo = time("Zip", { minEnergy(previous).zipWithIndex.min._2 })
    //val bar = time("IOF", { minEnergy(previous).indexOf(minEnergy(previous).min) })
    //val startAt = time("MINDEX", {minEnergy(previous).minIndex()})

    val startAt = minEnergy(previous).minIndex()

    // now follow the nodeTo array from bottom to top.
    val rv = Array.ofDim[Int](image.height)
    rv(image.height - 1) = startAt
    var xIdx = image.pixels(startAt)(image.height - 1).minNodeTo
    for (i <- image.height - 2 to 0 by -1) {
      rv(i) = image.pixels(xIdx)(i + 1).minNodeTo
      xIdx = rv(i)
    }
    rv

  }

  /**
   * Return a sequence of indices for horizontal seam
   *
   * @return sequence of indices for horizontal seam
   */
  def findHorizontalSeam(): HorizontalSeam = {

    sweepDirt()

    val minEnergy = Array.ofDim[Double](2, image.height)

    var current = 0
    var previous = 1
    image.foreachColumnOrderedWithIndices((p, x, y) => {
      if (x != 0) {
        p.minNodeTo = y
        minEnergy(current)(y) = minEnergy(previous)(y)
        if ((y != 0) && (minEnergy(previous)(y - 1) < minEnergy(current)(y))) {
          p.minNodeTo = y - 1
          minEnergy(current)(y) = minEnergy(previous)(y - 1)
        }
        if ((y != (image.height - 1)) && (minEnergy(previous)(y + 1) < minEnergy(current)(y))) {
          p.minNodeTo = y + 1
          minEnergy(current)(y) = minEnergy(previous)(y + 1)
        }
      }
      // add this guy's energy to whatever
      // we have so far while traversing.
      minEnergy(current)(y) += p.energy

      if (y == image.height - 1) {
        current ^= 1
        previous ^= 1
      }
    })

    // find minimum to start
    var startAt = 0
    var min = Double.MaxValue
    for (i <- 0 until image.height) {
      if (minEnergy(previous)(i) < min) {
        startAt = i
        min = minEnergy(previous)(i)
      }
    }
    // now follow the nodeTo array from left to right.
    val rv = new Array[Int](image.width)
    rv(image.width - 1) = startAt
    var yIdx = image.pixels(image.width - 1)(startAt).minNodeTo
    for (i <- image.width - 2 to 0 by -1) {
      rv(i) = image.pixels(i + 1)(yIdx).minNodeTo
      yIdx = rv(i)
    }

    rv
  }

  def printEnergy: Unit = {
    image.foreachPixel((p, x, y) => {
      print(image.pixels(x)(y).energy.toInt.toString.padTo(6, " ").mkString(""))
      print(" ")
      if (p.x == image.width - 1) {
        println("")
      }
    })
  }

  /**
   * Remove horizontal seam from image.
   *
   * @param a
   *          array of vertical indices. seam indices
   */
  def removeHorizontalSeam(seam: HorizontalSeam): Unit = {

    sweepDirt()

    if ((image.width <= 1) || (image.height <= 1)) {
      throw new IllegalArgumentException("image cannot support seam removal")
    }
    if (seam.length != image.width) {
      throw new IllegalArgumentException("invalid seam length")
    }

    image.horizontalDirt = Some(seam.zipWithIndex.map {
      case (s, i) => {
        for (y <- s until image.height - 1) {
          image.pixels(i)(y) = image.pixels(i)(y + 1)
          image.pixels(i)(y).y = y
        }
        (i, Range(s, image.pixels(i).length))
      }
    })
    image.height -= 1
  }

  /**
   * Remove vertical seam from picture
   *
   * @param a
   *          seam indices
   */
  def removeVerticalSeam(seam: VerticalSeam) {

    sweepDirt()

    if ((image.width <= 1) || (image.height <= 1)) {
      throw new IllegalArgumentException("image cannot support seam removal")
    }
    if (seam.length != image.height) {
      throw new IllegalArgumentException("invalid seam length")
    }

    val dirty = new ArrayBuffer[(Int, Range)]()
    for (y <- 0 until image.height) {
      dirty += ((y, Range(seam(y), image.width - 1)))
      for (x <- seam(y) until image.width - 1) {
        image.pixels(x)(y) = image.pixels(x + 1)(y)
        image.pixels(x)(y).x = x
      }
    }
    image.width -= 1

    image.verticalDirt = Some(dirty.toArray)
    image.cascadedDirtDirection = Direction.Vertical
    image.cascadedDirtPoint = (seam(0), 0)
  }

  def sweepDirt(): Unit = {
    image.horizontalDirt.getOrElse(Array()).foreach(item => {
      item._2.foreach(y => {
        image.pixels(item._1)(y).energy = energy(image, image.pixels(item._1)(y))
      })
    })
    image.horizontalDirt = None

    image.verticalDirt.getOrElse(Array()).foreach(item => {
      item._2.foreach(x => {
        image.pixels(x)(item._1).energy = energy(image, image.pixels(x)(item._1))
      })
    })
    image.verticalDirt = None
    
  }

}