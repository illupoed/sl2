package de.tuberlin.uebb.sl2.modules

trait ModuleNormalizer {
  this: Syntax with Type with ModuleResolver =>

  /**
   * undocumented
   */
  def normalizeModules( imports: List[ResolvedImport] ): List[ResolvedImport]

  /**
   * undocumented
   */
  def qualifyUnqualifiedModules( program: Program, imports: List[ResolvedImport] ): Program

}
