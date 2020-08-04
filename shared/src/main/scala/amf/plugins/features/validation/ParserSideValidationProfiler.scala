package amf.plugins.features.validation

import amf.ProfileName
import amf.core.validation._
import amf.core.validation.core.{SeverityMapping, ValidationProfile}

import scala.collection.mutable

object ParserSideValidationProfiler {

  private val cache: mutable.Map[ProfileName, ValidationProfile] = mutable.Map.empty

  def parserSideValidationsProfile(profile: ProfileName): ValidationProfile = {
    cache.get(profile) match {
      case Some(vp) => vp
      case _ =>
        val vp = computeProfile(profile)
        cache.put(profile, vp)
        vp
    }

  }

  private def computeProfile(profile: ProfileName) = {
    // sorting parser side validation for this profile
    val levels          = Seq(SeverityLevels.VIOLATION, SeverityLevels.INFO, SeverityLevels.WARNING)
    val severityMapping = SeverityMapping()
    levels.foreach { level =>
      val validations = Validations.validations.toStream
        .filter { v =>
          Validations
            .level(v.id, profile) == level
        }
        .map(_.name)
      severityMapping.set(validations, level)

    }

    ValidationProfile(
        name = ProfileName("Parser side AMF Validation"),
        baseProfile = None,
        severities = severityMapping,
        validations = Validations.validations
    )
  }

}
