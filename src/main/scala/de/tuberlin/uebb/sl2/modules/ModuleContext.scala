package de.tuberlin.uebb.sl2.modules

trait ModuleContext {
  this: Syntax with Context with ModuleResolver =>

  /**
   * undocumented
   */
  def buildModuleContext( imp: List[ResolvedImport] ): ( Context, Map[Var, FunctionSig] )

}
