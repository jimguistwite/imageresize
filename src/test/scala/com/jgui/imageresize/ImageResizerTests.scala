/*
*
* Copyright (c) 2013 James Guistwite. All Rights Reserved.
*
*/

package com.jgui.imageresize

import org.junit.Assert._
import org.junit.Test
import javax.imageio.ImageIO
import java.io.File
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("Large images should resize properly") {
    Array("F1", "F5").foreach(nm => {
      val in = new File(s"src/test/data/${nm}.jpg")
      val vout = new File(s"src/test/data/${nm}v.jpg")
      val hout = new File(s"src/test/data/${nm}h.jpg")
      val img = ImageIO.read(in)
      val vsc = new ImageResizer(img)

      time("Total Vertical", {
        for (i <- 0 until 100) {
          var seam: vsc.VerticalSeam = null
          seam = vsc.findVerticalSeam()
          vsc.removeVerticalSeam(seam)
        }
        ImageIO.write(vsc.getUpdatedImage(), "jpg", vout)
        val vimg = ImageIO.read(vout)
        assert(vimg.getHeight() == img.getHeight())
        assert(vimg.getWidth() == img.getWidth() - 100)
      })

      val hsc = new ImageResizer(img)

      time("Total Horizontal", {
        for (i <- 0 until 100) {
          var seam: hsc.HorizontalSeam = null
          seam = hsc.findHorizontalSeam()
          hsc.removeHorizontalSeam(seam)
        }
        ImageIO.write(hsc.getUpdatedImage(), "jpg", hout)
        val himg = ImageIO.read(hout)
        assert(himg.getHeight() == img.getHeight() - 100)
        assert(himg.getWidth() == img.getWidth())
      })
    })
  }
}

