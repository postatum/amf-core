package amf.core.validation.core

import amf.ProfileName
import amf.core.validation.SeverityLevels
import amf.core.validation.core.ValidationProfile.{SeverityLevel, ValidationName}

import scala.collection.mutable

case class ValidationProfile(name: ProfileName,
                             baseProfile: Option[ProfileName],
                             validations: Seq[ValidationSpecification],
                             severities: SeverityMapping,
                             prefixes: mutable.Map[String, String] = mutable.Map.empty) {

  def index: ProfileIndex = ProfileIndex(this)

  // TODO: Should we do this?
  def validate(): Unit = {
    // 1. Cannot define severity for unmatched specification
    // 2. Cannot define multiple severities for same validation
  }

  def validationsWith(severity: SeverityLevel): Seq[ValidationName] = {
    severity match {
      case SeverityLevels.INFO      => severities.info
      case SeverityLevels.WARNING   => severities.warning
      case SeverityLevels.VIOLATION => severities.violation
    }
  }
}

case class SeverityMapping() {
  // TODO: merge this in a single map
  var violation: Seq[ValidationName] = Seq.empty
  var warning: Seq[ValidationName]   = Seq.empty
  var info: Seq[ValidationName]      = Seq.empty
  var disabled: Seq[ValidationName]  = Seq.empty
  var default: SeverityLevel         = SeverityLevels.VIOLATION

  def set(validations: Seq[ValidationName], severity: SeverityLevel): this.type = {
    severity match {
      case SeverityLevels.INFO      => info = validations
      case SeverityLevels.WARNING   => warning = validations
      case SeverityLevels.VIOLATION => violation = validations
    }
    this
  }

  def disable(validations: Seq[ValidationName]): this.type = {
    disabled = validations
    this
  }
}

object ValidationProfile {
  // Circumvent no-typing
  type ValidationName = String
  type ValidationIri  = String
  type SeverityLevel  = String
}

case class ProfileIndex(profile: ValidationProfile) {

  val parents: Map[ValidationName, Seq[ValidationSpecification]] = {
    case class ParentChildPair(parent: ValidationSpecification, child: ValidationName)
    profile.validations.toStream
      .filter(_.nested.isDefined)
      .map { parent =>
        val child = parent.nested.get
        ParentChildPair(parent, child)
      }
      .groupBy(_.child)
      .mapValues { pairs =>
        pairs.map(_.parent)
      }
  }
}
