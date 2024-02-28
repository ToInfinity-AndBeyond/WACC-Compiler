package wacc

import scala.collection.mutable
import ast._
import Types._
import WaccError._

// Define the SymbolTable class to manage variable and function symbols within scopes
class SymbolTable(val parentScope: Option[SymbolTable]) {
  
  // A map to hold the current scope's symbols with their names as keys and their types as values
  private val currentScope: mutable.Map[String, Type] = mutable.Map()
  // Generate a unique identifier for functions to avoid name clashes
  private def generateUniqueIdent(name: String): String = {
    val prefix = "0_"
    val uniqueIdent = s"$prefix$name"
    uniqueIdent
  }

  // Insert a variable into the current scope
  def insert(name: String, pos: (Int, Int), literalType: Type): Either[List[WaccError], String] = {
    if (currentScope.contains(name)) {
      // Return an error if the variable is already defined in this scope
      Left(List(ScopeError(None, None, None, Some(s"Variable '$name' is already defined in this scope"), pos)))
    } else {
      // Add the variable to the current scope and return its name
      currentScope += (name -> literalType)
      Right(name)
    }
  }

  // Insert a function into the current scope with a unique identifier
  def insertFunc(name: String, pos: (Int, Int), literalType: FuncType): Either[List[WaccError], String] = {
    val uniqueIdent = generateUniqueIdent(name)
    if (currentScope.contains(uniqueIdent)) {
      // Return an error if the function is already defined in this scope
      Left(List(ScopeError(Some("Function declaration"), None, None, 
                           Some(s"Function '$name' is already defined in this scope"), pos)))
    } else {
      // Add the function to the current scope with its unique identifier and return the identifier
      currentScope += (uniqueIdent -> literalType)
      Right(uniqueIdent)
    }
  }

  // Lookup a variable or function in the current scope or parent scopes
  def lookUp(name: String, pos: (Int, Int)): Either[List[WaccError], Type] = {
    currentScope.get(name) match {
      // If found in the current scope, return its type
      case Some(t) => Right(t)
      // If not found, recurse up to the parent scope
      case None => parentScope match {
        case Some(parent) => parent.lookUp(name, pos) 
        // If no parent, return an error
        case None => Left(List(ScopeError(None, Some(s"${name}"), None, 
                          Some(s"Variable ${name} not declared in this scope"), pos))) 
      }
    }
  }

  // Lookup a function in the current scope or parent scopes using its unique identifier
  def lookUpFunc(name: String, pos: (Int, Int)): Either[List[WaccError], Type] = {
    // Generate the unique identifier for the function
    val fName = generateUniqueIdent(name) 
    // Perform the lookup with the unique identifier
    this.lookUp(fName, pos) match {
      case Left(err) => Left(List(ScopeError(None, Some(s"${name}"), None, 
                          Some(s"Variable ${name} not declared in this scope"), pos))) 
      case Right(t) => Right(t)
    }
  }
}