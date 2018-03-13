package com.huginnmuninnresearch.datatypes

case class Complex(re: Double, im: Double = 0) {

  /** Returns the result of adding another Complex to this Complex type.*/
  def +(c: Complex): Complex = new Complex(re + c.re, im + c.im)
  /** Returns the result of adding a real Double to this Complex type.*/
  def +(r: Double): Complex = new Complex(re + r, im)
  /** Returns the result of subtracting another Complex from this Complex type.*/
  def -(c: Complex): Complex = new Complex(re - c.re, im - c.im)
  /** Returns the result of subtracting a real Double from this Complex type.*/
  def -(r: Double): Complex = new Complex(re - r, im)
  /** Returns the result of multiplying this Complex by another Complex type.*/
  def *(c: Complex): Complex = new Complex(re*c.re - im*c.im, re*c.im + im*c.re)
  /** Returns the result of multiplying this Complex by a real Double type.*/
  def *(r: Double): Complex = new Complex(r*re, r*im)
  /** Returns the result of dividing this Complex by another Complex type.*/
  def /(c: Complex): Complex = new Complex((re*c.re + im*c.im) / c.norm, (im*c.re - re*c.im)/ c.norm)
  /** Returns the result of dividing this Complex by a real Double type.*/
  def /(r: Double): Complex = new Complex(re / r, im/ r)
  /** Returns the negative of this Complex type.*/
  def unary_- : Complex = new Complex(-re, -im)
  /** Returns the complex conjugate of this Complex type.*/
  def conj: Complex = new Complex(re, -im)
  /** Returns this Complex type multiplied by its conjugate.*/
  def norm: Double = re*re + im*im
  /** Returns the square root of the norm. This is the amplitude r of this Complex type represented in polar coordinates.*/
  def abs: Double = Math.sqrt(norm)
  /** Returns the String representation of a Complex type.*/
  override def toString: String = s"(Re: $re, Im: $im)"

}

object Complex {

  def apply(re: Double, im: Double = 0): Complex = {
    new Complex(re, im)
  }

  val i = new Complex(0, 1)

  def main(args: Array[String]): Unit = {
    val c1: Complex = new Complex(1, 2)
    println(c1.conj + new Complex(3, 4))
    println(c1 == new Complex(2, 2))
    println(c1- i*3)
    println(c1.norm)
  }
}
