package ruby

import scala.language.implicitConversions
import scala.language.dynamics

import org.jruby.{RubySymbol, RubyHash, RubyArray, Ruby=>RR, RubyObject, RubyString, RubyProc, RubyBoolean}
import org.jruby.runtime.{Block, BlockCallback, Arity, ThreadContext, CallBlock19}
import org.jruby.runtime.builtin.IRubyObject

trait Ruby[A <: AbstractRubyWrapper] extends Dynamic with Castings[A] {
  protected lazy val runtime = rubyObject.getRuntime
  protected lazy val context = runtime.getCurrentContext

  protected def __toRubyWrapper(a: IRubyObject): A

  def init(params: Any*) = send("new", params: _*)

  override def equals(other: Any): Boolean = other match {
    case r: AbstractRubyWrapper => send("==", r.rubyObject).rubyObject.toJava(classOf[Boolean]).asInstanceOf[Boolean]
    case _ => send("==", other).rubyObject.toJava(classOf[Boolean]).asInstanceOf[Boolean]
  }

  def selectDynamic(method: String) = send(method)

  def applyDynamicNamed(method: String)(params: (String, Any)*) = {
    var unbound: List[Any] = Nil
    var kv = params.foldLeft(Map[String, Any]()) { (map, param) =>
      if(param._1 == "") {
        unbound = unbound :+ param._2
        map
      } else {
        map + param
      }
    }
    send(method, unbound :+ kv: _*)
  }

  def +(params: Any*) = send("+", params: _*)
  def apply(param: Any) = send("[]", param)
  def update(key: Any, value: Any) = send("[]=", key, value)

  def applyDynamic(method: String)(params: Any*) = send(method, params: _*)
  def updateDynamic(method: String)(param: Any) = send(method + "=", param)

  def send(method: String, params: Any*): A = {
    val sent = rawSend(normalizedMethod(method), params: _*)
    __toRubyWrapper(sent)
  }

  def privateSend(method: String, params: Any*): A = {
    val sent = rawPrivateSend(normalizedMethod(method), params: _*)
    __toRubyWrapper(sent)
  }

  private def normalizedMethod(methodName: String) =
    if(methodName.endsWith("_!")) "_!$".r.replaceFirstIn(methodName, "!")
    else if(methodName.endsWith("_?")) "_\\?$".r.replaceFirstIn(methodName, "?")
    else methodName

  override def toString = rawSend("inspect").toString
  def to_s = rawSend("to_s").toString

  def rawSend(method: String, params: Any*): IRubyObject = {
    val normalized = normalize(method :: params.toList).asInstanceOf[IRubyObject]
    rubyObject.callMethod(context, Ruby.ScalaSend, Array(normalized), Block.NULL_BLOCK)
  }

  def rawPrivateSend(method: String, params: Any*): IRubyObject = {
    val normalized = normalize(method :: params.toList).asInstanceOf[IRubyObject]
    rubyObject.callMethod(context, Ruby.ScalaSend, Array(normalized, RubyBoolean.createTrueClass(runtime)), Block.NULL_BLOCK)
  }

  private def normalize(a: Any): Any = a match {
    case r: AbstractRubyWrapper => r.rubyObject
    case s: Symbol => RubySymbol.newSymbol(runtime, s.name)
    case (s1, s2) => normalize(Map( ( normalize(s1), normalize(s2) )  ))
    case s: Map[_, _] =>
      val hash = RubyHash.newHash(runtime)
      s.foreach { case(k, v) =>
        hash.put(normalize(k), normalize(v))
      }
      hash
    case s: Seq[_] =>
      val array = RubyArray.newArray(runtime)
      s.foreach { e =>
        array.add(normalize(e))
      }
      array
    case fn: Function0[Any] => createProc(0) { list => fn() }
    case fn: Function1[Any, Any] => createProc(1) { list => fn(list.head) }
    case fn: Function2[Any, Any, Any] => createProc(2) { case List(p1, p2, _*) => fn(p1, p2) }
    case fn: Function3[Any, Any, Any, Any] => createProc(3) { case List(p1, p2, p3, _*) => fn(p1, p2, p3) }
    case fn: Function4[Any, Any, Any, Any, Any] => createProc(4) { case List(p1, p2, p3, p4, _*) => fn(p1, p2, p3, p4) }
    case fn: Function5[Any, Any, Any, Any, Any, Any] => createProc(5) { case List(p1, p2, p3, p4, p5, _*) => fn(p1, p2, p3, p4, p5) }
    case _ => a
  }

  private def createProc(arity: Int)(fn: List[Any] => Any) = {
    val callback = new BlockCallback {
      def call(a: ThreadContext, args: Array[IRubyObject], block: org.jruby.runtime.Block) = {
        val normalized = args.map(Ruby(_).any).toList
        val result = fn(normalized)
        rawSend(Ruby.ScalaWrap, result)
      }
    }

    val cb = CallBlock19.newCallClosure(rubyObject, rubyObject.getSingletonClass, Arity.fixed(arity), callback, context)
    RubyProc.newProc(runtime, cb, Block.Type.NORMAL)
  }
}

object Ruby {
  lazy val env = {
    val e = RR.getGlobalRuntime
    e.evalScriptlet("""class Object
      def __scalaSend(params, priv=false)
        *args, block = params
        if block.is_a?(Proc)
          if priv
            send(*args, &block)
          else
            public_send(*args, &block)
          end
        else
          if(priv)
            send(*params)
          else
            public_send(*params)
          end
        end
      end;
    end""")

    e.evalScriptlet("module Kernel; def __wrap(param); param; end; end")

    e
  }

  lazy val ScalaHelper = env.evalScriptlet("""
    Class.new do
      def self.send(params)
        obj, *args = params
        obj.send(*args)
      end

      def self.wrap(obj)
        obj.first
      end
    end
  """).asInstanceOf[RubyObject]

  protected val ScalaSend = "__scalaSend"
  protected val ScalaWrap = "__wrap"

  def require(file: String) = env.evalScriptlet("require '" + file.replaceAll("'", "\\'") + "'")
  def apply(clazz: String) = new RubyImpl(env.evalScriptlet(clazz).asInstanceOf[RubyObject])
  def apply(obj: IRubyObject) = new RubyImpl(obj.asInstanceOf[RubyObject])
  //implicit def rb2boolean(ruby: Ruby[_]): Boolean = ruby.as(Boolean)

  def wrap(obj: Any) = {
    val arr = RubyArray.newArray(env)
    arr.add(obj)
    ScalaHelper.callMethod(context, "wrap", arr).asInstanceOf[RubyObject]
  }
  private val context = env.getCurrentContext

  class CastException(msg: String) extends Exception(msg)
}
