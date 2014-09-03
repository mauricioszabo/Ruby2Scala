package active_record

import ruby.Ruby

trait Migration extends Ruby[Migration] {
  def version: String

  def rubyObject: org.jruby.RubyObject = ???

  protected def __toRubyWrapper(a: org.jruby.runtime.builtin.IRubyObject): active_record.Migration = {
    val ro = rubyObject
    val ver = version
    new Migration {
      override def rubyObject = ro
      def version = ver
    }
  }
}
