package de.tuberlin.uebb.sl2.modules

/**
 * undocumented
 */
trait SignatureSerializer {
  this: Syntax =>

  /**
   * undocumented
   */
  def serialize( in: AST ): String

  /**
   * undocumented
   */
  def deserialize( in: String, location: Location = NoLocation ): AST

}