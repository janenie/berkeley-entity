package edu.berkeley.nlp.entity.wikivec

import java.io._

import edu.berkeley.nlp.entity.FloatArray

import scala.collection.mutable

/**
 * Created by matthewfl
 *
 * System for reading in word2vec representations when generated using `-binary 1`
 */
class w2vReader(val fname: String, val negvectors: Boolean = true) {

  /*private val bbuf = {
    val file = new RandomAccessFile(fname, "r")
    file.getChannel.map(FileChannel.MapMode.READ_ONLY, 0, file.length())
  }

  private val fbuf = new BufferFloatArray(bbuf.asFloatBuffer(), "", bbuf.limit() / 4)
*/

  private def flipEnding(i: Int) = (i&0xff)<<24 | (i&0xff00)<<8 | (i&0xff0000)>>8 | (i>>24)&0xff

  val (fmap, vdim, numwords) = {
    val fin = new FileInputStream(new File(fname))
    val bbuf = new DataInputStream(new BufferedInputStream(fin))

    val hm = new mutable.HashMap[String, (FloatArray, FloatArray) ]()
    val cbuf = new StringBuilder()
    val sizeSpec = bbuf.readLine().split(" ")
    val vdim = Integer.valueOf(sizeSpec(1))
    val numwords = Integer.valueOf(sizeSpec(0))
    try {
      while (true) {
        val c = bbuf.readByte()
        if (c == ' ') {
          // we have reached the end of a word
          val a = FloatArray.makeDiskArray(vdim) //new Array[Float](vdim)
          val b = if(negvectors) FloatArray.makeDiskArray(vdim) else null //if (negvectors) new Array[Float](vdim) else null
          var i = 0
          while (i < vdim) {
            val v = java.lang.Float.intBitsToFloat(flipEnding(bbuf.readInt()))
            if(v > 100 || v < -100)
              throw new RuntimeException();
            if(v == v)
              a(i) = v //bbuf.readFloat()
            i += 1
          }
          if (negvectors) {
            i = 0
            while (i < vdim) {
              val v = java.lang.Float.intBitsToFloat(flipEnding(bbuf.readInt()))
              // there should be no numbers outside of this range
              if(v > 100 || v < -100)
                throw new RuntimeException();
              if(v == v)
                b(i) = v //bbuf.readFloat()
              i += 1
            }
          }
          hm += (cbuf.toString -> (a, b))
          assert(bbuf.readByte() == '\n') // read new line char
          cbuf.clear()
        } else {
          cbuf.append(c.asInstanceOf[Char])
        }
      }
    } catch { case e: EOFException => {} }
    (hm.toMap, vdim, numwords)
  }

  def getVector(name: String) = {
    fmap.getOrElse(name, (Array[Float](), null))._1
  }

  def getContextV(context: Seq[String]): Array[Float] = {
    val c = new Array[Float](vdim)
    var cnt = 0
    for(w <- context) {
      val wv = fmap.getOrElse(w, null)
      if (wv != null) {
        var i = 0
        cnt += 1
        while (i < vdim) {
          c(i) += wv._1(i)
          i += 1
        }
      }
    }
    if(cnt > 0) {
      var i = 0
      while (i < vdim) {
        c(i) /= cnt
        i += 1
      }
    }
    c
  }

  def computeP(word: String, context: Array[Float]): Double = {
    val v = fmap.getOrElse(word, null)
    if(v == null) return 0.0
    var innerp: Double = 0.0
    var i = 0
    while(i < vdim) {
      innerp += v._2(i).asInstanceOf[Double] * context(i).asInstanceOf[Double]
      i += 1
    }
    innerp
  }

  //def computeDistance()

}


object w2vReader {

  def main(args: Array[String]): Unit = {
    println(args(1))
    val vecs = new w2vReader(args(1))
    for(w <- vecs.fmap) {
      val fa = w._2
      for(i <- 0 until vecs.vdim) {
        if(fa._1(i) > 70 || fa._2(i) > 70) {
          println("failed")
          return
        }
      }
    }
  }

}