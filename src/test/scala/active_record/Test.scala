package test.active_record

import org.scalatest._
import active_record._
import ruby.Ruby

class Test extends WordSpec with matchers.ShouldMatchers {
}

trait AR extends BeforeAndAfterAll { self: Suite =>
  override def beforeAll {
    String.synchronized {
      Ruby require "rubygems"
      Ruby require "active_record"
      Ruby apply """
        class CreatePeople < ActiveRecord::Migration
          def self.up
            create_table :people do |t|
              t.string :name
              t.integer :age
              t.text :custom_data
              t.timestamps
            end
          end
        end

        class CreateChildren < ActiveRecord::Migration
          def self.up
            create_table :children do |t|
              t.string :name
              t.integer :age
              t.integer :person_id
              t.timestamps
            end
          end
        end
      """

      Model.connect('adapter -> "jdbcsqlite3", 'database -> ":memory:")

      Ruby apply """
        CreatePeople.up
        CreateChildren.up
      """
    }
  }
}
