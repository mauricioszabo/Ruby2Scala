package ruby

import scala.language.implicitConversions
import scala.language.dynamics

import org.jruby.{RubySymbol, RubyHash, RubyArray, Ruby=>RR, RubyObject, RubyString}
import org.jruby.runtime.{Block, BlockCallback, CallBlock, Arity, ThreadContext}
import org.jruby.runtime.builtin.IRubyObject

class RubyImpl(val rubyObject: RubyObject) extends Ruby[RubyImpl] {
  protected def __toRubyWrapper(ir: IRubyObject) = new RubyImpl(ir.asInstanceOf[RubyObject])
}
