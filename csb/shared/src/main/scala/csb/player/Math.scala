package csb.player

import math.{ atan2, sqrt, pow, cos, sin, Pi }

case class Angle(radian: Double) {
  // internal representation in radian from -Pi to Pi.
  def +(other: Angle) = Radian(radian + other.radian)
  def -(other: Angle) = Radian(radian - other.radian)
  def *(scale: Double) = Radian(radian * scale)
  // compare absolute angle
  def <(other: Angle): Boolean =
    if (radian * radian < other.radian * other.radian) true
    else false
  // compare absolute angle
  def >(other: Angle): Boolean =
    if (radian * radian > other.radian * other.radian) true
    else false
  def unary_- = Radian(-radian)
  def degree: Double = radian / Pi * 180
  override def toString = s"Degree($degree)"
}

object Degree {
  def apply(degree: Double) = Angle.fromDegree(degree)
}

object Radian {
  def apply(radian: Double) = Angle.fromRadian(radian)
}

object Angle {
  def fromDegree(angle: Double) = Radian(angle / 180 * Pi)
  def fromRadian(radian: Double): Angle = {
    val r = radian % (2 * Pi)
    if (r <= -Pi) new Angle(r + 2 * Pi)
    else if (r > Pi) new Angle(r - 2 * Pi)
    else new Angle(r)
  }
}

case class Point(x: Double, y: Double) {
  override def toString = s"Point($x,$y)"
  def +(other: Point) = Point(x + other.x, y + other.y)
  def -(other: Point) = Point(x - other.x, y - other.y)
  def unary_- = Point(-x, -y)
  def *(factor: Double) = Point(x * factor, y * factor)
  def distanceTo(other: Point): Double =
    sqrt(pow(x - other.x, 2) + pow(y - other.y, 2))
  def toInt = Array(x.toInt, y.toInt)
  def normalize: Point = if (this == Point(0, 0)) this
  else Point(x / norm, y / norm)
  def scalar(other: Point): Double = x * other.x + y * other.y
  def squareNorm = x * x + y * y
  lazy val norm = sqrt(squareNorm)
  def radianWith(other: Point) =
    Radian(atan2(y, x) - atan2(other.y, other.x))
  def radianFrom(other: Point) =
    Radian(atan2(other.y, other.x) - atan2(y, x))
  def rotate(angle: Angle): Point = {
    val ca = cos(angle.radian)
    val sa = sin(angle.radian)
    Point(ca * x - sa * y, sa * x + ca * y)
  }
  def round = Point(x.round, y.round)
  // floor a 2D vector in absolute norm
  def floor = Point(if (x < 0) x.ceil else x.floor, if (y < 0) y.ceil else y.floor)
  def data: Array[Double] = Array(x, y)
}

object Point extends {
  def apply(a: Int, b: Int): Point = Point(a.toDouble, b.toDouble)
}

