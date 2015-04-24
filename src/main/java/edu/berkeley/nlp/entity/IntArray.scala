package edu.berkeley.nlp.entity

import java.io.{RandomAccessFile, ObjectInputStream, ObjectOutputStream}
import java.nio.channels.FileChannel
import java.nio.{IntBuffer, MappedByteBuffer}

/**
 * Created by matthewfl
 */

trait IntArray extends Iterable[Int] with Serializable  {

  def apply(i: Int) : Int
  def update(i: Int, v: Int) : Unit
  def length : Int
  override def size = length

  override def iterator = new intIter(this)
}

class intIter (val arr: IntArray) extends Iterator[Int] {
  private var at = 0
  override def hasNext = at < arr.length
  override def next = {
    val v = arr(at)
    at += 1
    v
  }
}


@SerialVersionUID(1L)
class RawIntArray (private val arr: Array[Int]) extends IntArray {
  override def apply(i: Int) = arr(i)
  override def update(i: Int, v: Int) = {
    arr(i) = v
  }
  override def length = arr.length
}

@SerialVersionUID(1L)
class SubIntArray (private val arr: IntArray, private val start: Int, val length: Int) extends IntArray {
  override def apply(i: Int) = arr(start + i)
  override def update(i: Int, v: Int) = arr(start + i) = v
}

// note: do not construct this class directly....
class BufferIntArray (ib: IntBuffer, private var bufferName: String, val length: Int) extends IntArray {

  @transient
  private var intBuffer: IntBuffer = ib

  private def writeObject(os: ObjectOutputStream) = {
    os.defaultWriteObject()
    os.writeObject(bufferName)
  }

  private def readObject(is: ObjectInputStream) = {
    is.defaultReadObject()
    bufferName = is.readObject().asInstanceOf[String]

    val file = new RandomAccessFile(bufferName, "rw")
    val buffer = file.getChannel.map(FileChannel.MapMode.READ_WRITE, 0, length * 4)
    intBuffer = buffer.asIntBuffer()
  }

  override def apply(i: Int) = {
    intBuffer.get(i)
  }

  override def update(i: Int, v: Int) = {
    intBuffer.put(i, v)
  }

}



object IntArray {

  // A single run will all end up having the same date time so we can copy them easier
  val dateTime = {
    val d = new java.util.Date
    d.getTime.toString
  }

  def makeArray(length: Int): IntArray = {
    new RawIntArray(new Array[Int](length))
  }

  def combineArraysRaw(arrs: Seq[IntArray]): Seq[IntArray] = {
    val totalLength = arrs.map(_.length).sum
    val backingArr = new RawIntArray(new Array[Int](totalLength))
    var at = 0
    for(a <- arrs) yield {
      val start = at
      for(v <- a) {
        backingArr(at) = v
        at += 1
      }
      new SubIntArray(backingArr, start, a.length)
    }
  }

  def combineArraysMapped(arrs: Seq[IntArray]): Seq[IntArray] = {
    val totalLength = arrs.map(_.length).sum
    val bufferName = s"IntArray-${dateTime}-${java.util.UUID.randomUUID().toString.replace("-","")}"
    val file = new RandomAccessFile(bufferName, "rw")
    val buffer = file.getChannel.map(FileChannel.MapMode.READ_WRITE, 0, totalLength * 4)
    val intBuffer = buffer.asIntBuffer()
    val backingArr = new BufferIntArray(intBuffer, bufferName, totalLength)
    var at = 0
    val ret = for(a <- arrs) yield {
      val start = at
      for(v <- a) {
        backingArr(at) = v
        at += 1
      }
      new SubIntArray(backingArr, start, a.length)
    }
    buffer.force()
    ret
  }

}