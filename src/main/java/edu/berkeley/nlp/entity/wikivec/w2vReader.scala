package edu.berkeley.nlp.entity.wikivec

import java.io._

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

  val (fmap, vdim, numwords) = {
    val bbuf = new DataInputStream(new BufferedInputStream(new FileInputStream(new File(fname))))
    val hm = new mutable.HashMap[String, (Array[Float], Array[Float]) ]()
    val cbuf = new StringBuilder()
    val sizeSpec = bbuf.readLine().split(" ")
    val vdim = Integer.valueOf(sizeSpec(1))
    val numwords = Integer.valueOf(sizeSpec(0))
    try {
      while (true) {
        val c = bbuf.readByte()
        if (c == ' ') {
          // we have reached the end of a word
          val a = new Array[Float](vdim)
          val b = if (negvectors) new Array[Float](vdim) else null
          var i = 0
          while (i < vdim) {
            a(i) = bbuf.readFloat()
            i += 1
          }
          if (negvectors) {
            i = 0
            while (i < vdim) {
              b(i) = bbuf.readFloat()
              i += 1
            }
          }
          hm += (cbuf.toString ->(a, b))
          bbuf.readByte() // read new line char
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

  //def computeDistance()

}
