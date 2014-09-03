package ruby

import scala.language.dynamics

import org.jruby.{RubySymbol, RubyHash, RubyArray, Ruby=>RR, RubyObject, RubyString}
import org.jruby.runtime.{Block, BlockCallback, CallBlock, Arity, ThreadContext}
import org.jruby.runtime.builtin.IRubyObject

trait DynamicDispatch[A] extends Dynamic {
  protected val __toRubyWrapper: IRubyObject => A

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

  private def normalizedMethod(methodName: String) =
    if(methodName.endsWith("_!")) "_!$".r.replaceFirstIn(methodName, "!")
    else if(methodName.endsWith("_?")) "_\\?$".r.replaceFirstIn(methodName, "?")
    else methodName

  //override def toString = rawSend("inspect").toString
  //def to_s = rawSend("to_s").toString
  def rawSend(method: String, params: Any*): IRubyObject
}
