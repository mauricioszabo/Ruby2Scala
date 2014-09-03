package ruby

import scala.language.implicitConversions
import java.math.{BigInteger, BigDecimal => BD}
import java.lang.{Long=>JLong, Double=>JDouble}
import org.jruby._

trait Castings[A <: AbstractRubyWrapper] extends AbstractRubyWrapper {
  import collection.JavaConversions._

  def send(method: String, params: Any*): A

  def any: Any = normalize(rubyObject)

  private def normalize(obj: Any): Any = obj match {
    case s: RubySymbol => Symbol(s.toString)
    case bd: BD => bd : BigDecimal
    case bn: BigInteger => bn : BigInt
    case r: RubyRegexp => regexp(r)
    case l: RubyArray => list(l).toVector
    case h: RubyHash => map(h)
    case e: RubyString => e.decodeString: String
    case e: RubyFixnum => e.getLongValue
    case e: RubyFloat => e.getValue
    case e: RubyBignum => e.getValue: BigInt
    case e: ext.bigdecimal.RubyBigDecimal => e.getValue: BigDecimal
    case e: RubyBoolean => e.isTrue
    case _ => obj
  }

  def as[A](implicit m: Manifest[A]): Option[A] = any match {
    case e: JLong if(m.toString == "Int") => Some(e.toInt.asInstanceOf[A])
    case e: JLong if(m.toString == "Long") => Some(e.asInstanceOf[A])
    case e: JDouble if(m.toString == "Float") => Some(e.toFloat.asInstanceOf[A])
    case e: JDouble if(m.toString == "Double") => Some(e.asInstanceOf[A])
    case m(e) => Some(e)
    case _ => None
  }

  def as(param: String.type): String = castTo[String]
  def as(param: Int.type): Int = castTo[JLong].toInt
  def as(param: Long.type): Long = castTo[JLong]
  def as(param: Float.type): Float = castTo[JDouble].toFloat
  def as(param: Double.type): Double = castTo[JDouble]

  def as(param: Regex.type) = castTo[scala.util.matching.Regex]
  def as(param: BigInt.type) = castTo[BigInt]
  def as(param: BigDecimal.type) = castTo[BigDecimal]
  def as(param: Symbol.type) = castTo[Symbol]
  def as(param: Map.type) = castTo[Map[Any, Any]]
  def as(param: List.type) = castTo[Vector[Any]].toList
  def as(param: Vector.type) = castTo[Vector[Any]]

  def as(param: Boolean.type) = rubyObject.isTrue

  private def cast[A, B](fn: A => B)(implicit m: Manifest[A]): B = rubyObject match {
    case m(o) => fn(o)
    case _ => error(m.runtimeClass.getSimpleName)
  }

  private def castTo[A](implicit m: Manifest[A]): A = any match {
    case m(obj) => obj
    case _ => error(m.runtimeClass.getSimpleName)
  }

  private def error(rbType: String) =
    throw new Ruby.CastException("expected " + rbType + " but got " + send("class").toString)

  private def regexp(r: RubyRegexp) = r.source.toString.r
  private def list(h: RubyArray) = h.map(normalize)
  private def map(h: RubyHash) = h.map { case(k, v) => normalize(k) -> normalize(v) }.toMap
}

object Castings {
  implicit def castings2bool(c: Castings[_]) = c.as(Boolean)
}
