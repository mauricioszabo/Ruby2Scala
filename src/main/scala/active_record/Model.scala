package active_record

import ruby.Ruby
import org.jruby.RubyObject
import org.jruby.runtime.builtin.IRubyObject

class Model extends Ruby[Model] {
  lazy val rubyClass = Ruby(
    "class " + rbClassName + " < ActiveRecord::Base\n" +
    "end\n" + rbClassName
  )
  lazy val rubyObject: IRubyObject = rubyClass.rubyObject

  def __toRubyWrapper(r: IRubyObject) = new Model {
    override lazy val rubyObject = r
  }

  def create(attributes: (Symbol, Any)*): Model = super.create(Map(attributes: _*))
  def init(attributes: (Symbol, Any)*): Model = super.init(Map(attributes: _*))

  private lazy val rbClassName = "[\\$\\.]".r.split(className).last
  private lazy val className = getClass.getName

  def scope(name: Symbol, condition: => Model) {
    val sc = rubyClass.singleton_class
    sc.privateSend("define_method", name, () => condition)
  }

  def scope(name: Symbol, condition: Any => Model) {
    val sc = rubyClass.singleton_class
    sc.privateSend("define_method", name, condition)
  }
}

object Model {
  Ruby require "active_record"
  def connect(params: (Symbol, Any)*) {
    val arBase = Ruby("ActiveRecord::Base")
    arBase.establish_connection(Map(params: _*))
  }
}
