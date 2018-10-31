package org.adridadou.openlaw.parser.template.expressions

import org.adridadou.openlaw.parser.template.{Compare, TemplateExecutionResult, VariableName}

trait BinaryExpression extends Expression {

  def left:Expression
  def right:Expression

  override def missingInput(executionResult: TemplateExecutionResult): Either[String, Seq[VariableName]] = for {
      leftMissing <- left.missingInput(executionResult)
      rightMissing <- right.missingInput(executionResult)
    } yield leftMissing ++ rightMissing

  override def validate(executionResult: TemplateExecutionResult): Option[String] = {
    val leftType = left.expressionType(executionResult)
    val rightType = right.expressionType(executionResult)
    if(!leftType.isCompatibleType(rightType, Compare)) {
      Some("left and right expression need to be of the same type to be computed." + leftType.name + " & " + rightType.name + " in " + left.toString + " & " + right.toString)
    } else {
      (for {
        _ <- left.missingInput(executionResult)
        _ <- right.missingInput(executionResult)
      } yield Unit) match {
        case Left(ex) =>
          Some(ex)
        case Right(_) =>
          None
      }
    }
  }


  override def variables(executionResult: TemplateExecutionResult): Seq[VariableName] =
    left.variables(executionResult) ++ right.variables(executionResult)
}
