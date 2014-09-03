package ruby

import scala.util.Random

import org.jruby.javasupport.Java
import org.jruby.{RubyObject, RubyClass, MetaClass}
import org.jruby.runtime.builtin.{IRubyObject, InstanceVariables, InternalVariables, Variable}
import org.jruby.runtime.{ThreadContext, Block}
import java.lang.reflect.Method
import org.jruby.runtime.Helpers

class Subclass(className: String, params: Any*) extends Ruby[RubyImpl] {
  import collection.JavaConversions._

  lazy val superObject = Ruby(className).init(params: _*).rubyObject

  def rubyObject = {
    val wrappedRubyClass = Java.getProxyClassForObject(superObject.getRuntime, this)
    wrappedRubyClass.setSuperClass(superObject.getType)
    val wrapped = Java.allocateProxy(this, wrappedRubyClass).asInstanceOf[RubyObject]

    superObject.getInstanceVariableList.toList.foreach { variable =>
      wrapped.setInstanceVariable(variable.getName, variable.getValue)
    }
    wrapped
  }

  private lazy val myMethods = getClass.getMethods.foldLeft(Map[String, List[Method]]()) { (map, method) =>
    map.updated(method.getName, method :: map.get(method.getName).getOrElse(Nil))
  }

  def __toRubyWrapper(obj: IRubyObject) = new RubyImpl(obj.asInstanceOf[RubyObject])
}
