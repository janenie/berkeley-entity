package edu.berkeley.nlp.entity

import java.io.{RandomAccessFile, ObjectInputStream, ObjectOutputStream}
import java.nio.channels.FileChannel
import java.nio.{IntBuffer, MappedByteBuffer}

import scala.collection.mutable

/**
 * Created by matthewfl
 */

trait IntArray extends mutable.ArrayLike[Int, Array[Int]] /*with IndexedSeq[Int]*/ with Serializable  {

  def apply(i: Int) : Int
  def update(i: Int, v: Int) : Unit
  def length : Int

  // just copied from ofInt class from scala,
  // I guess don't try using some complicated += method as it will likely break
  // this makes method such as sorted or sortBy not work directly, so use toArray first
  override protected[this] def thisCollection: mutable.WrappedArray[Int] = {
    new mutable.WrappedArray.ofInt(toArray)
  }
  override protected[this] def toCollection(repr: Array[Int]): mutable.WrappedArray[Int] = {
    new mutable.WrappedArray.ofInt(toArray)
  }
  override protected[this] def newBuilder : mutable.Builder[Int,Array[Int]] = {
    println("IntArray get newBuilder")
    throw new NotImplementedError()
    null
  }

  override def seq = new IntArraySeq(this)

  def toArray = super.toArray

}

class IntArraySeq (val arr: IntArray) extends IndexedSeq[Int] {
  override def apply(i: Int) = arr(i)
  override def length = arr.length
}


@SerialVersionUID(1L)
class RawIntArray (val arr: Array[Int]) extends IntArray {
  override def apply(i: Int) = arr(i)
  override def update(i: Int, v: Int) = {
    arr(i) = v
  }
  override def length = arr.length

  // this is an optimization to avoid copying all the elements
  // however it means that after toArray they are still backed by the same array
  // whereas in cases of the other subclasses they won't be backed, so the behavior is
  // inconsistent
  override def toArray = arr
}

@SerialVersionUID(1L)
class SubIntArray (private val arr: IntArray, private val start: Int, val length: Int) extends IntArray {
  override def apply(i: Int) = arr(start + i)
  override def update(i: Int, v: Int) = arr(start + i) = v
}

// note: do not construct this class directly....
@SerialVersionUID(1L)
class BufferIntArray (ib: IntBuffer, private var bufferName: String, private var lengthv: Int) extends IntArray {

  @transient
  private var intBuffer: IntBuffer = ib

  private def writeObject(os: ObjectOutputStream) = {
    os.defaultWriteObject()
    os.writeObject(bufferName)
    os.writeInt(lengthv)
  }

  private def readObject(is: ObjectInputStream) = {
    is.defaultReadObject()
    bufferName = is.readObject().asInstanceOf[String]
    lengthv = is.readInt()

    val file = new RandomAccessFile(IntArray.prefixDir + "/" + bufferName, "rw")
    val buffer = file.getChannel.map(FileChannel.MapMode.READ_WRITE, 0, lengthv * 4)
    intBuffer = buffer.asIntBuffer()

    // stupid hack to make sure that we have read all the values at least once
    // so that when we randomly access them they should all already be cached
    var ss = 0
    for(i <- 0 until lengthv)
      ss += intBuffer.get(i)
  }

  override def apply(i: Int) = {
    intBuffer.get(i)
  }

  override def update(i: Int, v: Int) = {
    intBuffer.put(i, v)
  }

  override def length = lengthv

}



object IntArray {

  // A single run will all end up having the same date time so we can copy them easier
  val dateTime = {
    val d = new java.util.Date
    d.getTime.toString
  }

  var prefixDir = "."

  val empty = makeArray(0)

  implicit def makeArray(arr: Array[Int]): IntArray = {
    new RawIntArray(arr)
  }

  implicit def backToPrimitive(arr: IntArray): Array[Int] = {
    arr match {
      case a: RawIntArray => a.arr
      case a => a.toArray
    }
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

  // construct one or more single files that back a sequence of arrays
  def combineArraysMapped(arrs: Seq[IntArray]): Seq[IntArray] = {
    val totalLength: Long = arrs.map(_.length.asInstanceOf[Long]).sum
    var backingArr : BufferIntArray = null //makeBackingArr(totalLength)
    var at = 0
    var consumed: Long = 0
    val ret = for(a <- arrs) yield {
      // we are going to limit the size of the file to 1gb to avoid potential scaling issues
      if(backingArr == null || (at + a.length) > backingArr.length) {
        val remaining = totalLength - consumed
        backingArr = makeBackingArr(Math.max(a.length, Math.min(remaining, 1024*1024*256).asInstanceOf[Int]))
        at = 0
      }
      consumed += a.length
      val start = at
      for(v <- a) {
        backingArr(at) = v
        at += 1
      }
      new SubIntArray(backingArr, start, a.length)
    }

    ret
  }

  // add the current array to the current running buffer for the arrays
  // a buffer will be up to 2gb in size
  def makeDiskBacked(arr: IntArray): IntArray = {
    if(sharedBacking == null || (sharedConsumed + arr.length) > sharedBacking.length) {
      sharedBacking = makeBackingArr(Math.max(arr.length, 1024*1024*500))
      sharedConsumed = 0
    }
    val start = sharedConsumed
    for(v <- arr) {
      sharedBacking(sharedConsumed) = v
      sharedConsumed += 1
    }
    new SubIntArray(sharedBacking, start, arr.length)
  }


  private var sharedBacking: BufferIntArray = null
  private var sharedConsumed: Int = 0

  private def makeBackingArr(length: Int) = {
    val bufferName = s"IntArray-${dateTime}-${java.util.UUID.randomUUID().toString.replace("-","")}"
    val file = new RandomAccessFile(prefixDir + "/" + bufferName, "rw")
    val buffer = file.getChannel.map(FileChannel.MapMode.READ_WRITE, 0, length.asInstanceOf[Long] * 4)
    val intBuffer = buffer.asIntBuffer()
    new BufferIntArray(intBuffer, bufferName, length)
  }

}