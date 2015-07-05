package edu.berkeley.nlp.entity

import java.io.{ObjectInputStream, ObjectOutputStream, RandomAccessFile}
import java.nio.FloatBuffer
import java.nio.channels.FileChannel

import scala.collection.mutable

/**
 * Created by matthewfl
 */
sealed trait FloatArray extends mutable.ArrayLike[Float, Array[Float]] /*with IndexedSeq[Int]*/ with Serializable {

  def apply(i: Int) : Float
  def update(i: Int, v: Float) : Unit
  def length : Int

  // just copied from ofInt class from scala,
  // I guess don't try using some complicated += method as it will likely break
  // this makes method such as sorted or sortBy not work directly, so use toArray first
  override protected[this] def thisCollection: mutable.WrappedArray[Float] = {
    new mutable.WrappedArray.ofFloat(toArray)
  }
  override protected[this] def toCollection(repr: Array[Float]): mutable.WrappedArray[Float] = {
    new mutable.WrappedArray.ofFloat(toArray)
  }
  override protected[this] def newBuilder : mutable.Builder[Float,Array[Float]] = {
    println("IntArray get newBuilder")
    ???
  }

  override def seq = new FloatArraySeq(this)

  def toArray = super.toArray

}

class FloatArraySeq(val arr: FloatArray) extends mutable.IndexedSeq[Float] {
  override def apply(i: Int) = arr(i)
  override def update(i: Int, v: Float) = arr(i) = v
  override def length = arr.length
}

@SerialVersionUID(1L)
class RawFloatArray (val arr: Array[Float]) extends FloatArray {
  override def apply(i: Int) = arr(i)
  override def update(i: Int, v: Float) = {
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
class SubFloatArray (private val arr: FloatArray, private val start: Int, val length: Int) extends FloatArray {
  override def apply(i: Int) = arr(start + i)
  override def update(i: Int, v: Float) = arr(start + i) = v
}

@SerialVersionUID(1L)
class BufferFloatArray (ib: FloatBuffer, private var bufferName: String, private var lengthv: Int) extends FloatArray {

  @transient
  private var floatBuffer: FloatBuffer = ib

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
    floatBuffer = buffer.asFloatBuffer()

    // stupid hack to make sure that we have read all the values at least once
    // so that when we randomly access them they should all already be cached
    buffer.load()
  }

  override def apply(i: Int) = {
    floatBuffer.get(i)
  }

  override def update(i: Int, v: Float) = {
    floatBuffer.put(i, v)
  }

  override def length = lengthv

}

object FloatArray {

  implicit def makeArray(arr: Array[Float]) = new RawFloatArray(arr)

  implicit def backToPrimitive(arr: FloatArray) = arr.toArray

  // TODO: other methods copy from IntArray

}