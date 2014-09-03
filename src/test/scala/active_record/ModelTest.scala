package test.active_record

class ModelTest extends Test with AR {
  object Person extends active_record.Model {
    scope('foo, this.where('name -> "Foo"))
    scope('age, a => this.where('age -> a))

    this has_many 'children
  }

  object Child extends active_record.Model {
    scope('ageOne, this.where('age -> 1))
    this belongs_to 'person
  }

  "Model" should {
    "define a new model in AR" in {
      val p = Person.init('name -> "Name")
      p.name should be === "Name"
    }

    "create and save a model" in {
      val p = Person.create('name -> "A Name")
      p.name should be === "A Name"
    }

    "chain where conditions" in {
      createPeople
      val underaged = Person.where('name -> "Foo").where('age -> 17)
      underaged.size should be === 1
    }

    "be indexed" in {
      createPeople
      val ps = Person.where('name -> "Bar")
      ps(0).name should be === "Bar"
    }

    "allow scopes" in {
      createPeople
      var underaged = Person.age(17)
      underaged = underaged.foo

      underaged.size should be === 1
      underaged(0).name should be === "Foo"
      underaged(0).age should be === 17
    }

    "associate records" in {
      createPeople
      val child = Child.first
      val person = child.person

      person.children.ageOne.first.name should be === "Bar Daughter"
    }
  }

  def createPeople {
    Person.delete_all
    Child.delete_all
    Person.create('name -> "Foo", 'age -> 17)
    Person.create('name -> "Foo", 'age -> 18)
    val p = Person.create('name -> "Bar", 'age -> 18)
    Child.create('name -> "Bar Son", 'age -> 2, 'person_id -> p.id)
    Child.create('name -> "Bar Daughter", 'age -> 1, 'person_id -> p.id)
  }
}
