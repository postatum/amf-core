package amf.core.traversal.iterator
import amf.core.metamodel.document.FragmentModel
import amf.core.model.document.Document
import amf.core.model.document.FieldsFilter.Local
import amf.core.model.domain.{AmfObject, DomainElement}
import com.mule.data.DataNodes
import org.scalatest.{FunSuite, Matchers}

trait IteratorTest extends FunSuite with Matchers {

  test("Complete iterator (simple document)") {
    val it = AmfElementStrategy.iterator(List(DataNodes.document))
    it.size should be(19)
  }

  test("Complete iterator (recursive fragment)") {
    val it = AmfElementStrategy.iterator(List(DataNodes.fragment))
    it.size should be(14)
  }

  test("Domain element iterator (recursive fragment) collect") {
    val ids =
      DataNodes.fragment.iterator(strategy = DomainElementStrategy).collect { case e: DomainElement => e.id }.toStream

    ids should contain inOrderOnly ("amf://recursive", "amf://name", "amf://age")
  }

  test("Amf element iterator (recursive fragment) collect first") {
    val location = AmfElementStrategy.iterator(List(DataNodes.fragment)).collectFirst {
        case obj: AmfObject if obj.fields.?(FragmentModel.Location).isDefined =>
          obj.fields.get(FragmentModel.Location).toString
      }

    location shouldBe Some("http://fragment")
  }

  test("Domain element iterator (complex document) with local fields") {
    val complex: Document = DataNodes.complex
    val ids = complex.iterator(strategy = DomainElementStrategy, fieldsFilter = Local).collect { case e: DomainElement => e.id }.toStream
    ids should contain inOrderOnly ("amf://name", "amf://age", "amf://happy")
  }


}
