/*
 * Copyright (c) 2012, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

package de.tuberlin.uebb.sl2.impl

import scala.collection.mutable.ListBuffer
import de.tuberlin.uebb.sl2.modules._
import de.tuberlin.uebb.sl2.modules.Syntax.{VarFirstClass}

import language.experimental.macros
import reflect.macros.{Context => MacroCtxt}
import scala.io.Source
import java.io.File
import java.security.MessageDigest
import scalax.io._

/**
  * Driver for the `slc' macro.
  */
object MacroDriver
	extends CombinatorParser
	with MultiDriver
	with Configs
	with AbstractFile
  	with CodeGenerator
  	with Syntax
  	with ProgramChecker
  	with JsSyntax
  	with Errors
  	with SignatureSerializer
  	with DebugOutput
  	with ModuleResolver
  	with ModuleNormalizer
  	with ModuleLinearization
	with PreProcessing 
	with NameSupply 
	with Graph[VarFirstClass]
	with Context
	with Type
	with SyntaxTraversal
	with EnrichedLambdaCalculus 
	with Substitution
	with Unification
	
	//TODO why the fuck I have to include these things without knowing it by default ... 
    with Lexic
    with GraphImpl[VarFirstClass]
    with LetRecSplitter
    with DTCheckerImpl
    with FDCheckerImpl
    with TypeCheckerImpl
    with ProgramCheckerImpl
    with SignatureJsonSerializer
    with ModuleResolverImpl
    with ModuleNormalizerImpl
    with ModuleContextImpl
{
	
	def macroImpl(c : MacroCtxt)(s : c.Expr[String]) : c.Expr[String] = 
	{
		import c.universe._
		
		s match 
	    {
	    	case Expr(Literal(Constant(sl))) => 
	    	{
	    		val assets_dir		= "/home/norcain/workspace/sl2-demo/public/sl/"
	    		val generator_dir	= "generated/"
	    		val module_name		= md5(showRaw(c))
	    		val filename		= generator_dir + module_name + ".sl"
	    		
	    		val target_file		= new File(assets_dir + filename);
	    		
	    		if (target_file.exists())
	    			target_file.delete()
	    		
	    		
	    		val tmp_file:Output = Resource.fromFile(assets_dir + filename)
	    		
	    		tmp_file.write(sl.toString)(Codec.UTF8)
	    		
	    		//build config
	    		val config: Config = Config	( new File(assets_dir) // source path
	    									, filename :: List() // sources
	    									, new File(assets_dir) // class path
	    									, "" // mainName
	    									, new File("") // mainParent
	    									, new File(assets_dir) // destination
	    									, true // compile for play framework
	    									)
	    		
	    		val res = run(config)
	    		
	    		if (res.isLeft)
	    		{
	    			//TODO return errors
	    			res.left.map(x => println("Errors:\n" + x))
	    			c.abort(c.enclosingPosition, "Error")
	    		}
	    		else
	    			c.Expr(Literal(Constant(renderRequire(module_name) + "\n")))
	    	}
	    	case _ => 
	    	{
	    		c.abort(c.enclosingPosition, "Expected a string literal, got: " + showRaw(s))
	    	}
	    }
		
		//c.Expr(Literal(Constant("alert(\"hallo world\");" + "\n")))
	}
	
	def md5(input: String): String =
	{
		val ar = MessageDigest.getInstance("MD5").digest(input.getBytes())
		
		ar.foldLeft("")((x,y) => x + String.format("%x", new Integer(y & 0xff)))
	}
	
	def slci(s : String) = macro macroImpl
	
	def renderRequire(module_name: String): String =
	{
		String.format(
"""
require
	( [ "generated/%s.sl" ]
	, function (foo)
		{
			foo.$main()
		}
	);
"""
				, module_name
				)
	}

	/*val prelude = Source.fromURL(getClass.getResource("/prelude.sl")).mkString
	val preludeJs = Source.fromURL(getClass.getResource("/prelude.js")).mkString

	def macroImpl(c : MacroCtxt)(s : c.Expr[String]) : c.Expr[String] = 
	{
		import c.universe._
		
	    s match 
	    {
	    	case Expr(Literal(Constant(sl))) => 
	    	{
	    		val result = run(prelude::sl.toString::Nil)
	    		
	    		result match 
	    		{
	    			case Left(LocatedError(msg, FileLocation(_, from, to))) => 
	    			{
	    				val point = c.enclosingPosition.startOrPoint + from.col + 1
	    						c.abort(c.enclosingPosition.withPoint(point), msg)
	    			}
	    			case Left(e) => c.abort(c.enclosingPosition, e.toString)
	    			case Right(js) => c.Expr(Literal(Constant(preludeJs + "\n" + js)))
	    		}
	    	}
	    	case _ => 
	    	{
	    		c.abort(c.enclosingPosition, "Expected a string literal, got: " + showRaw(s))
	    	}
	    }
	}

	def slci(s : String) = macro macroImpl

	def slcImpl(c : MacroCtxt)(s : c.Expr[String]) : c.Expr[String] = 
	{
		import c.universe._
		
		s match 
		{
			case Expr(Literal(Constant(sl))) => 
			{
				val res = MacroDriver.getClass.getResource(sl.toString)
				(	
					for (url <- Option(res)) yield 
					{
						val file = Source.fromURL(url).mkString
						val result = run(prelude::file::Nil)
						result match 
						{
							case Left(LocatedError(msg, FileLocation(f, from, to))) => 
							{
								val fileMsg = "%s: %d:%d\n%s".format(f,from.line,from.col,msg)
								c.abort(c.enclosingPosition, fileMsg)
							}
							case Left(e) => c.abort(c.enclosingPosition, e.toString)
							case Right(js) => c.Expr(Literal(Constant(preludeJs + "\n" + js)))
						}
					}
				).getOrElse(c.abort(c.enclosingPosition, "Could not load source file : " + sl + " from classpath."))
			}
			case _ => 
			{
				c.abort(c.enclosingPosition, "Expected a string literal, got: " + showRaw(s))
			}      
		}
	}

	def slc1Impl[T : c.WeakTypeTag](c : MacroCtxt)(s : c.Expr[String]) : c.Expr[String] = 
	{
	    import c.universe._
	    
	    s match
	    {
	    	case Expr(Literal(Constant(sl))) => 
	    	{
	    		val res = MacroDriver.getClass.getResource(sl.toString)
	    		(
	    			for (url <- Option(res)) yield 
	    			{
	    				val conv = new Scala2Sl(c.universe, Map("Double" -> "Real"), this)
	    				conv.scala2SlType(Map())(weakTypeOf[T].asInstanceOf[conv.universe.Type])
	    				val model = conv.currentProgram.toString
	
	    				val file = Source.fromURL(url).mkString
	    				val result = run(prelude::model::file::Nil)
	    				
	    				result match 
	    				{
	    					case Left(LocatedError(msg, FileLocation(f, from, to))) => 
	    					{
	    						val fileMsg = "%s: %d:%d\n%s".format(f,from.line,from.col,msg)
	    						c.abort(c.enclosingPosition, fileMsg)
	    					}
	    					case Left(e) => c.abort(c.enclosingPosition, e.toString)
	    					case Right(js) => c.Expr[String](Literal(Constant(preludeJs + "\n" + js)))
	    				}
	    			}
	    		).getOrElse(c.abort(c.enclosingPosition, "Could not load source file : " + sl + " from classpath."))
	    	}
	    	case _ => 
	    	{
	    		c.abort(c.enclosingPosition, "Expected a string literal, got: " + showRaw(s))
	    	}      
	    }
	}

	def slc(s : String) = macro slcImpl

	def slc1[T](s : String) = macro slc1Impl[T]

	override def run(input: List[String]): Either[Error, String] =
    {
		val ast: ListBuffer[Program] = new ListBuffer
		
		for (f <- input) 
		{
			parseAst(f) match 
			{
				case Right(a) => ast += a.asInstanceOf[Program]
				case Left(a) => return Left(a)
			}
		}
      	
		val m = ast.foldLeft[Either[Error, Program]](Right(Program(Map(), Map(), Nil)))((z, x) =>
        z.right.flatMap(y => mergeAst(y, x)))
        
        for ( mo <- m.right
        	; _ <- checkProgram(mo).right
        	) yield
        {
        	JsPrettyPrinter.pretty(astToJs(mo) & JsFunctionCall("$main"))
        }
    }

	def mergeAst(a: Program, b: Program): Either[Error, Program] =
    {
		for ( sigs <- mergeMap(a.signatures, b.signatures).right
			; funs <- mergeMap(a.functionDefs, b.functionDefs).right
			) yield 
		{
			val defs = a.dataDefs ++ b.dataDefs
			Program(sigs, funs, defs)
		}
    }

	def mergeMap[A, B](a: Map[A, B], b: Map[A, B]): Either[Error, Map[A, B]] =
    {
		val intersect = a.keySet & b.keySet
		
		if (intersect.isEmpty)
			Right(a ++ b)
		else
			Left(DuplicateError("Duplicated definition: " + intersect.mkString(", "), "", Nil))
    }*/
}
