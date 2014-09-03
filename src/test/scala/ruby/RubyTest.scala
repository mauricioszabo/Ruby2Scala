package test.ruby
import ruby._
import org.jruby.javasupport.Java

class RubyTest extends test.active_record.Test {
  val testCode = """
    class SomeClass
      attr_reader :params
      attr_accessor :name

      def initialize(params={})
        @name = params[:name]
        @params = params
      end

      def save!
        if @name
          true
        else
          raise ArgumentError, "some error"
        end
      end

      def dependend_of_name
        name.upcase
      end

      def self.three
        yield 1
        yield 2
        yield 3
      end

      def self.num(value = 1)
        value.times.map { |x| yield x }
      end
    end
    SomeClass
  """

  val test = Ruby(testCode)

  "Ruby castings" should {
    "cast to Any" in {
      Ruby("10").any should be === 10
      Ruby("10").any should be === 10
      Ruby("10.2").any should be === 10.2
      Ruby("'a string'").any should be === "a string"
      Ruby("true").any should be === true
    }

    "return objects as their scala counterparts" in {
      Ruby("true") as Boolean should be === true
      Ruby("10") as Int should be === 10
      Ruby("10") as Long should be === 10
      Ruby("10.2") as Float should be === 10.2f
      Ruby("10.2") as Double should be === 10.2
      Ruby("'a string'") as String should be === "a string"
    }

    "automatic cast ruby objects to boolean" in {
      Ruby("true") should be === true
      Ruby("false") should be === false
      (Ruby("nil"): Boolean) should be === false
      (Ruby("Object.new"): Boolean) should be === true
    }

    "return ruby's complex objects as their scala counterparts" in {
      Ruby require "bigdecimal"

      Ruby("123456789123456789123456789") as BigInt should be === BigInt("123456789123456789123456789")
      Ruby("BigDecimal.new('10.3')") as BigDecimal should be === BigDecimal(10.3)
      Ruby(":foo") as Symbol should be === 'foo
      Ruby("[1, 2, 3]") as List should be === List(1, 2, 3)
      Ruby("[1, 2, 3]") as Vector should be === Vector(1, 2, 3)
      Ruby("{1 => 10, 2 => 20}") as Map should be === Map(1 -> 10, 2 -> 20)
    }

    "normalize ruby's collection" in {
      Ruby("{a: 10, b: 20}") as Map should be === Map('a -> 10, 'b -> 20)
      Ruby("[:a, BigDecimal.new('10.2'), 123456789123456789123456789]") as List should be === List(
        'a, BigDecimal("10.2"), BigInt("123456789123456789123456789") )

      Ruby("{ a: { b: [:c, :d, { e: [ :f ] }] }}") as Map should be === Map(
        'a -> Map('b -> Vector('c, 'd, Map('e -> Vector('f)))) )
    }

    "return ruby's regexp as scala regexps" in {
      val r = Ruby("/asd/") as Regex
      r.findFirstIn("asd") should be ('nonEmpty)
    }

    "return exception in case cast fails" in {
      intercept[Ruby.CastException] { Ruby("10") as String }
    }

    "checks if nil" in {
      val ten = Ruby("10")
      val nil = Ruby("nil")
      ten.as[Int] should be === Some(10)
      nil.as[Int] should be === None
    }

    "cast blocks to functions" is pending
  }

  "Ruby" should {
    val newTest = test.init('name -> "Foo")

    "cast a ruby object to a wrapper" in {
      val str = Ruby("10").rawSend("to_s")
      Ruby(str) should be === Ruby("'10'")
    }

    "instantiate a class" in {
      test.init('name -> "Foo").name == "Foo" should be (true)
    }

    "convert ! and ? methods" in {
      newTest.is_a_?(test.rubyObject) should be === true
      newTest.save_! should be === true
    }

    "dynamically call methods" in {
      newTest.name = "Bar"
      newTest.name should be === "Bar"
    }

    "un-wrap ruby objects when calling ruby from scala" in {
      newTest.is_a_?(test) should be === true
    }

    "match other classes" in {
      newTest.name.any match {
        case s: String =>
        case _ => fail("ERROR!")
      }
    }

    "maps [] and []=" in {
      val array = Ruby("[:a, :b]")
      array(0) should be === 'a
      array(0) = "Foo"
      array should be === List("Foo", 'b)
    }

    "map functions" in {
      var list: List[Long] = Nil
      test.three { i: Long =>
        list = i :: list
      }
      list should be === List(3, 2, 1)
    }

    "map functions and convert values" in {
      val list = test.num(4, {i: Long => i + 20})
      list should be === Vector(20, 21, 22, 23)
    }
  }

  "Ruby integration" should {
    "allow subclassing" in {
      val sub = new SubclassTest1("Foo Bar")
      sub.name should be === "Foo Bar"
    }

    "allow method overriding" in {
      val sub = new SubclassTest2
      sub.dependend_of_name should be === "ANOTHER NAME"
    }
  }
}

class SubclassTest1(n: String) extends Subclass("SomeClass", ('name -> n))

class SubclassTest2 extends Subclass("SomeClass", ('name -> "Foo")) {
  def name = "Another name"
}
