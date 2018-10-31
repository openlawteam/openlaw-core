package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.parser.template.expressions.Expression

case class StripeCall(
  beneficiary: Expression
)

case class StripeCallDetails(
  index: Int,
  call: StripeCall,
  description: String
)