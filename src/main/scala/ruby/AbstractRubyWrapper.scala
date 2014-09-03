package ruby

import org.jruby.RubyObject
import org.jruby.runtime.builtin.IRubyObject

trait AbstractRubyWrapper {
  protected[ruby] def rubyObject: IRubyObject
}
