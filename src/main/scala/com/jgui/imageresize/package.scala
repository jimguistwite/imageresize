package com.jgui

package object imageresize {
  
   // both consistently cost me 2-3ms
    def time[R](title: String, block: => R): R = {
      val t0 = System.currentTimeMillis()
      val result = block // call-by-name
      val t1 = System.currentTimeMillis()
      println(title + " elapsed time: " + (t1 - t0) + "ms")
      result
    }
    
}

