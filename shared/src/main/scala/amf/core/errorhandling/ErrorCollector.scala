package amf.core.errorhandling
import amf.core.validation.AMFValidationResult

import scala.collection.mutable

trait ErrorCollector extends AmfResultErrorHandler {
  private val errors: mutable.LinkedHashSet[AMFValidationResult] = mutable.LinkedHashSet()

  override def handlerAmfResult(result: AMFValidationResult): Boolean = synchronized {
    if(!errors.contains(result)) {
      errors += result
      true
    }else false
  }

  def getErrors: List[AMFValidationResult] =  errors.toList
}
