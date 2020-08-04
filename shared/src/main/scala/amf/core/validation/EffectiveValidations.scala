package amf.core.validation

import amf.core.validation.core.ValidationProfile.{SeverityLevel, ValidationIri}
import amf.core.validation.core.{ValidationProfile, ValidationSpecification}
import amf.core.vocabulary.Namespace

import scala.collection.mutable

class EffectiveValidations(val effective: mutable.HashMap[String, ValidationSpecification] = mutable.HashMap(),
                           val all: mutable.HashMap[String, ValidationSpecification] = mutable.HashMap()) {

  val levelsIndex: mutable.HashMap[ValidationIri, SeverityLevel] = mutable.HashMap.empty

  def findLevel(id: ValidationIri): Option[SeverityLevel] = levelsIndex.get(id)

  def update(other: ValidationSpecification): Unit = {
    all.get(other.name) match {
      case Some(added) => all.update(other.name, other withTargets added)
      case None        => all += other.name -> other
    }
  }

  def someEffective(profile: ValidationProfile): EffectiveValidations = {
    val index = profile.index

    // we aggregate all of the validations to the total validations map
    profile.validations.foreach { update }

    val levels = Seq(SeverityLevels.INFO, SeverityLevels.WARNING, SeverityLevels.VIOLATION)

    levels.foreach { level =>
      profile.validationsWith(level).foreach { validation =>
        setLevel(validation, level)
        index.parents.get(toIri(validation)) match {
          case Some(parents) =>
            parents.foreach { parent =>
              effective.put(parent.id, parent)
            }
          case _ => // Nothing
        }
      }
    }

    profile.severities.disabled foreach { id =>
      val validationIri: ValidationIri = toIri(id)
      this.effective.remove(validationIri)
    }
    this
  }

  def allEffective(specifications: Seq[ValidationSpecification]): EffectiveValidations = {
    specifications foreach { spec =>
      all += (spec.name       -> spec)
      effective += (spec.name -> spec)
      levelsIndex(spec.name) = SeverityLevels.VIOLATION
    }
    this
  }

  private def setLevel(id: String, targetLevel: SeverityLevel): Unit = {
    val validationIri: ValidationIri = toIri(id)
    all.get(validationIri) match {
      case Some(validation) =>
        levelsIndex.update(validationIri, targetLevel)
        effective += (validationIri -> validation)
      case None => // Ignore
    }
  }

  private def toIri(id: String): ValidationIri = {
    if (!isIri(id)) {
      Namespace.expand(id.replace(".", ":")).iri()
    }
    else {
      id
    }
  }

  private def isIri(id: String) = id.startsWith("https://") || id.startsWith("file:/") || id.startsWith("http://")

}

object EffectiveValidations {
  def apply() = new EffectiveValidations()
}
