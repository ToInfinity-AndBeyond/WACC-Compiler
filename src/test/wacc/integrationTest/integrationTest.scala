package integrationTest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.sys.process._
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._
import scala.collection.mutable.LinkedHashMap
import ANSIColors._
import wacc.ExitCodes._
import wacc.PrettyPrint

object ANSIColors {
  val ANSI_RESET = "\u001B[0m"
  val ANSI_RED   = "\u001B[31m"
  val ANSI_GREEN = "\u001B[32m"
}

class IntegrationTest extends AnyFlatSpec with Matchers {
  "Front End" should "process custom test valid cases with exit code 0" in {
    val folder = "src/test/tests/valid"
    runValidTests(getFiles(folder))
  }

  it should "process custom test syntaxError cases with exit code 100" in {
    val folder = "src/test/tests/syntaxError"
    runSyntaxErrorTests(getFiles(folder))
  }

  it should "process custom test semanticError cases with exit code 200" in {
    val folder = "src/test/tests/semanticError"
    runSemanticErrorTests(getFiles(folder))
  }

  it should "process valid/advanced test cases with exit code 0" in {
    val files = List(
      "test_cases/valid/advanced/binarySortTree.wacc",
      "test_cases/valid/advanced/hashTable.wacc",
      "test_cases/valid/advanced/ticTacToe.wacc"
    )
    runValidTests(files)
  }

  it should "process valid/array test cases with exit code 0" in {
    val files = List(
      "test_cases/valid/array/arrayLength.wacc",
      "test_cases/valid/array/lenArrayIndex.wacc",
      "test_cases/valid/array/arrayIndexMayBeArrayIndex.wacc",
      "test_cases/valid/array/emptyArrayReplace.wacc",
      "test_cases/valid/array/stringFromArray.wacc",
      "test_cases/valid/array/arrayEmpty.wacc",
      "test_cases/valid/array/array.wacc",
      "test_cases/valid/array/arrayLookup.wacc",
      "test_cases/valid/array/emptyArrayPrint.wacc",
      "test_cases/valid/array/free.wacc",
      "test_cases/valid/array/arrayOnHeap.wacc",
      "test_cases/valid/array/modifyString.wacc",
      "test_cases/valid/array/printRef.wacc",
      "test_cases/valid/array/emptyArrayNextLine.wacc",
      "test_cases/valid/array/arraySimple.wacc",
      "test_cases/valid/array/charArrayInStringArray.wacc",
      "test_cases/valid/array/arrayNested.wacc",
      "test_cases/valid/array/arrayPrint.wacc",
      "test_cases/valid/array/emptyArrayScope.wacc",
      "test_cases/valid/array/arrayBasic.wacc",
      "test_cases/valid/array/emptyArrayAloneIsFine.wacc"
    )
    runValidTests(files)
  }

  it should "process valid/pairs test cases with exit code 0" in {
    val files = List(
      "test_cases/valid/pairs/printNull.wacc",
      "test_cases/valid/pairs/printNullPair.wacc",
      "test_cases/valid/pairs/printPair.wacc",
      "test_cases/valid/pairs/nestedPair.wacc",
      "test_cases/valid/pairs/createRefPair.wacc",
      "test_cases/valid/pairs/free.wacc",
      "test_cases/valid/pairs/pairExchangeArrayOk.wacc",
      "test_cases/valid/pairs/writeSnd.wacc",
      "test_cases/valid/pairs/nestedPairLeftAssign.wacc",
      "test_cases/valid/pairs/writeFst.wacc",
      "test_cases/valid/pairs/printPairOfNulls.wacc",
      "test_cases/valid/pairs/null.wacc",
      "test_cases/valid/pairs/pairarray.wacc",
      "test_cases/valid/pairs/createPair02.wacc",
      "test_cases/valid/pairs/createPair.wacc",
      "test_cases/valid/pairs/nestedPairRightExtract.wacc",
      "test_cases/valid/pairs/checkRefPair.wacc",
      "test_cases/valid/pairs/createPair03.wacc",
      "test_cases/valid/pairs/readPair.wacc",
      "test_cases/valid/pairs/linkedList.wacc"
    )
    runValidTests(files)
  }

  it should "process valid/if test cases with exit code 0" in {
    val files = List(
      "test_cases/valid/if/if1.wacc",
      "test_cases/valid/if/whitespace.wacc",
      "test_cases/valid/if/ifFalse.wacc",
      "test_cases/valid/if/ifBasic.wacc",
      "test_cases/valid/if/if6.wacc",
      "test_cases/valid/if/ifTrue.wacc",
      "test_cases/valid/if/if5.wacc",
      "test_cases/valid/if/if4.wacc",
      "test_cases/valid/if/if3.wacc",
      "test_cases/valid/if/if2.wacc"
    )
    runValidTests(files)
  }

  it should "process valid/runtimeErr test cases with exit code 0" in {
    val files = List(
      "test_cases/valid/runtimeErr/integerOverflow/intJustOverflow.wacc",
      "test_cases/valid/runtimeErr/integerOverflow/intnegateOverflow.wacc",
      "test_cases/valid/runtimeErr/integerOverflow/intmultOverflow.wacc",
      "test_cases/valid/runtimeErr/integerOverflow/intnegateOverflow3.wacc",
      "test_cases/valid/runtimeErr/integerOverflow/intWayOverflow.wacc",
      "test_cases/valid/runtimeErr/integerOverflow/intnegateOverflow2.wacc",
      "test_cases/valid/runtimeErr/integerOverflow/intUnderflow.wacc",
      "test_cases/valid/runtimeErr/integerOverflow/intnegateOverflow4.wacc",
      "test_cases/valid/runtimeErr/nullDereference/readNull2.wacc",
      "test_cases/valid/runtimeErr/nullDereference/useNull2.wacc",
      "test_cases/valid/runtimeErr/nullDereference/setNull1.wacc",
      "test_cases/valid/runtimeErr/nullDereference/freeNull.wacc",
      "test_cases/valid/runtimeErr/nullDereference/setNull2.wacc",
      "test_cases/valid/runtimeErr/nullDereference/useNull1.wacc",
      "test_cases/valid/runtimeErr/nullDereference/readNull1.wacc",
      "test_cases/valid/runtimeErr/badChar/tooBigChr.wacc",
      "test_cases/valid/runtimeErr/badChar/negativeChr.wacc",
      "test_cases/valid/runtimeErr/divideByZero/divZero.wacc",
      "test_cases/valid/runtimeErr/divideByZero/divideByZero.wacc",
      "test_cases/valid/runtimeErr/divideByZero/modByZero.wacc",
      "test_cases/valid/runtimeErr/arrayOutOfBounds/arrayOutOfBoundsWrite.wacc",
      "test_cases/valid/runtimeErr/arrayOutOfBounds/arrayNegBounds.wacc",
      "test_cases/valid/runtimeErr/arrayOutOfBounds/arrayOutOfBounds.wacc"
    )
    runValidTests(files)

  }

  it should "process valid/IO test cases with exit code 0" in {
    val files = List(
      "test_cases/valid/IO/IOLoop.wacc",
      "test_cases/valid/IO/read/echoPuncChar.wacc",
      "test_cases/valid/IO/read/echoBigInt.wacc",
      "test_cases/valid/IO/read/echoInt.wacc",
      "test_cases/valid/IO/read/echoNegInt.wacc",
      "test_cases/valid/IO/read/echoChar.wacc",
      "test_cases/valid/IO/read/echoBigNegInt.wacc",
      "test_cases/valid/IO/read/readAtEof.wacc",
      "test_cases/valid/IO/read/read.wacc",
      "test_cases/valid/IO/IOSequence.wacc",
      "test_cases/valid/IO/print/printCharAsString.wacc",
      "test_cases/valid/IO/print/println.wacc",
      "test_cases/valid/IO/print/printEscChar.wacc",
      "test_cases/valid/IO/print/multipleStringsAssignment.wacc",
      "test_cases/valid/IO/print/print.wacc",
      "test_cases/valid/IO/print/hashInProgram.wacc",
      "test_cases/valid/IO/print/printBool.wacc",
      "test_cases/valid/IO/print/printCharArray.wacc",
      "test_cases/valid/IO/print/print-backspace.wacc",
      "test_cases/valid/IO/print/printInt.wacc",
      "test_cases/valid/IO/print/printChar.wacc"
    )
    runValidTests(files)
  }

  it should "process valid/basic test cases with exit code 0" in {
    val files = List(
      "test_cases/valid/basic/skip/skip.wacc",
      "test_cases/valid/basic/skip/commentEoF.wacc",
      "test_cases/valid/basic/skip/comment.wacc",
      "test_cases/valid/basic/skip/commentInLine.wacc",
      "test_cases/valid/basic/exit/exit-1.wacc",
      "test_cases/valid/basic/exit/exitBasic.wacc",
      "test_cases/valid/basic/exit/exitBasic2.wacc",
      "test_cases/valid/basic/exit/exitWrap.wacc"
    )
    runValidTests(files)
  }

  it should "process valid/scope test cases with exit code 0" in {
    val files = List(
      "test_cases/valid/scope/printAllTypes.wacc",
      "test_cases/valid/scope/splitScope.wacc",
      "test_cases/valid/scope/scopeSimpleRedefine.wacc",
      "test_cases/valid/scope/scopeWhileRedefine.wacc",
      "test_cases/valid/scope/scopeRedefine.wacc",
      "test_cases/valid/scope/indentationNotImportant.wacc",
      "test_cases/valid/scope/scopeIfRedefine.wacc",
      "test_cases/valid/scope/scope.wacc",
      "test_cases/valid/scope/scopeBasic.wacc",
      "test_cases/valid/scope/ifNested2.wacc",
      "test_cases/valid/scope/ifNested1.wacc",
      "test_cases/valid/scope/scopeVars.wacc",
      "test_cases/valid/scope/intsAndKeywords.wacc",
      "test_cases/valid/scope/scopeWhileNested.wacc"
    )
    runValidTests(files)
  }

  it should "process valid/function test cases with exit code 0" in {
    val files = List(
      "test_cases/valid/function/nested_functions/fixedPointRealArithmetic.wacc",
      "test_cases/valid/function/nested_functions/printTriangle.wacc",
      "test_cases/valid/function/nested_functions/printInputTriangle.wacc",
      "test_cases/valid/function/nested_functions/mutualRecursion.wacc",
      "test_cases/valid/function/nested_functions/fibonacciFullRec.wacc",
      "test_cases/valid/function/nested_functions/functionConditionalReturn.wacc",
      "test_cases/valid/function/nested_functions/simpleRecursion.wacc",
      "test_cases/valid/function/nested_functions/fibonacciRecursive.wacc",
      "test_cases/valid/function/simple_functions/argScopeCanBeShadowed.wacc",
      "test_cases/valid/function/simple_functions/manyArgumentsChar.wacc",
      "test_cases/valid/function/simple_functions/functionDeclaration.wacc",
      "test_cases/valid/function/simple_functions/usesArgumentWhilstMakingArgument.wacc",
      "test_cases/valid/function/simple_functions/sameArgName2.wacc",
      "test_cases/valid/function/simple_functions/lotsOfLocals.wacc",
      "test_cases/valid/function/simple_functions/functionManyArguments.wacc",
      "test_cases/valid/function/simple_functions/punning.wacc",
      "test_cases/valid/function/simple_functions/asciiTable.wacc",
      "test_cases/valid/function/simple_functions/functionSimple.wacc",
      "test_cases/valid/function/simple_functions/incFunction.wacc",
      "test_cases/valid/function/simple_functions/sameNameAsVar.wacc",
      "test_cases/valid/function/simple_functions/functionMultiReturns.wacc",
      "test_cases/valid/function/simple_functions/sameArgName.wacc",
      "test_cases/valid/function/simple_functions/negFunction.wacc",
      "test_cases/valid/function/simple_functions/functionUpdateParameter.wacc",
      "test_cases/valid/function/simple_functions/manyArgumentsInt.wacc",
      "test_cases/valid/function/simple_functions/functionIfReturns.wacc",
      "test_cases/valid/function/simple_functions/functionReturnPair.wacc",
      "test_cases/valid/function/simple_functions/functionDoubleReturn.wacc",
      "test_cases/valid/function/simple_functions/functionSimpleLoop.wacc"
    )
    runValidTests(files)
  }

  it should "process valid/variables test cases with exit code 0" in {
    val files = List(
      "test_cases/valid/variables/capCharDeclaration.wacc",
      "test_cases/valid/variables/negIntDeclaration.wacc",
      "test_cases/valid/variables/charDeclaration.wacc",
      "test_cases/valid/variables/emptyStringDeclaration.wacc",
      "test_cases/valid/variables/stringDeclaration.wacc",
      "test_cases/valid/variables/longVarNames.wacc",
      "test_cases/valid/variables/boolDeclaration.wacc",
      "test_cases/valid/variables/_VarNames.wacc",
      "test_cases/valid/variables/charDeclaration2.wacc",
      "test_cases/valid/variables/manyVariables.wacc",
      "test_cases/valid/variables/intDeclaration.wacc",
      "test_cases/valid/variables/boolDeclaration2.wacc",
      "test_cases/valid/variables/zeroIntDeclaration.wacc",
      "test_cases/valid/variables/puncCharDeclaration.wacc",
      "test_cases/valid/variables/stringCarriageReturn.wacc"
    )
    runValidTests(files)
  }

  it should "process valid/expressions test cases with exit code 0" in {
    val files = List(
      "test_cases/valid/expressions/intCalc.wacc",
      "test_cases/valid/expressions/multNoWhitespaceExpr.wacc",
      "test_cases/valid/expressions/andExpr.wacc",
      "test_cases/valid/expressions/equalsOverBool.wacc",
      "test_cases/valid/expressions/divExpr.wacc",
      "test_cases/valid/expressions/negBothMod.wacc",
      "test_cases/valid/expressions/longSplitExpr.wacc",
      "test_cases/valid/expressions/negDivisorDiv.wacc",
      "test_cases/valid/expressions/ordAndchrExpr.wacc",
      "test_cases/valid/expressions/lessExpr.wacc",
      "test_cases/valid/expressions/lessCharExpr.wacc",
      "test_cases/valid/expressions/equalsOverOr.wacc",
      "test_cases/valid/expressions/negDividendDiv.wacc",
      "test_cases/valid/expressions/equalsOverAnd.wacc",
      "test_cases/valid/expressions/longExpr2.wacc",
      "test_cases/valid/expressions/longSplitExpr2.wacc",
      "test_cases/valid/expressions/minusExpr.wacc",
      "test_cases/valid/expressions/intExpr1.wacc",
      "test_cases/valid/expressions/greaterEqExpr.wacc",
      "test_cases/valid/expressions/plusPlusExpr.wacc",
      "test_cases/valid/expressions/lessEqExpr.wacc",
      "test_cases/valid/expressions/multExpr.wacc",
      "test_cases/valid/expressions/longExpr3.wacc",
      "test_cases/valid/expressions/plusNoWhitespaceExpr.wacc",
      "test_cases/valid/expressions/plusMinusExpr.wacc",
      "test_cases/valid/expressions/negDividendMod.wacc",
      "test_cases/valid/expressions/longExpr.wacc",
      "test_cases/valid/expressions/boolExpr1.wacc",
      "test_cases/valid/expressions/equalsExpr.wacc",
      "test_cases/valid/expressions/negDivisorMod.wacc",
      "test_cases/valid/expressions/negBothDiv.wacc",
      "test_cases/valid/expressions/orExpr.wacc",
      "test_cases/valid/expressions/sequentialCount.wacc",
      "test_cases/valid/expressions/notequalsExpr.wacc",
      "test_cases/valid/expressions/negExpr.wacc",
      "test_cases/valid/expressions/andOverOrExpr.wacc",
      "test_cases/valid/expressions/stringEqualsExpr.wacc",
      "test_cases/valid/expressions/notExpr.wacc",
      "test_cases/valid/expressions/greaterExpr.wacc",
      "test_cases/valid/expressions/plusExpr.wacc",
      "test_cases/valid/expressions/charComparisonExpr.wacc",
      "test_cases/valid/expressions/modExpr.wacc",
      "test_cases/valid/expressions/minusPlusExpr.wacc",
      "test_cases/valid/expressions/minusMinusExpr.wacc",
      "test_cases/valid/expressions/boolCalc.wacc",
      "test_cases/valid/expressions/minusNoWhitespaceExpr.wacc"
    )
    runValidTests(files)
  }

  it should "process valid/sequence test cases with exit code 0" in {
    val files = List(
      "test_cases/valid/sequence/stringAssignment.wacc",
      "test_cases/valid/sequence/charAssignment.wacc",
      "test_cases/valid/sequence/intLeadingZeros.wacc",
      "test_cases/valid/sequence/boolAssignment.wacc",
      "test_cases/valid/sequence/intAssignment.wacc",
      "test_cases/valid/sequence/basicSeq.wacc",
      "test_cases/valid/sequence/basicSeq2.wacc",
      "test_cases/valid/sequence/exitSimple.wacc"
    )
    runValidTests(files)
  }

  it should "process valid/while test cases with exit code 0" in {
    val files = List(
      "test_cases/valid/while/whileBasic.wacc",
      "test_cases/valid/while/loopIntCondition.wacc",
      "test_cases/valid/while/rmStyleAddIO.wacc",
      "test_cases/valid/while/whileFalse.wacc",
      "test_cases/valid/while/fibonacciFullIt.wacc",
      "test_cases/valid/while/whileCount.wacc",
      "test_cases/valid/while/min.wacc",
      "test_cases/valid/while/max.wacc",
      "test_cases/valid/while/whileBoolFlip.wacc",
      "test_cases/valid/while/fibonacciIterative.wacc",
      "test_cases/valid/while/loopCharCondition.wacc",
      "test_cases/valid/while/rmStyleAdd.wacc"
    )
    runValidTests(files)
  }

  it should "process syntaxError/array test cases with exit code 100" in {
    val files = List(
      "test_cases/invalid/syntaxErr/array/arrayExpr.wacc"
    )
    runSyntaxErrorTests(files)
  }

  it should "process syntaxError/pairs test cases with exit code 100" in {
    val files = List(
      "test_cases/invalid/syntaxErr/pairs/sndNull.wacc",
      "test_cases/invalid/syntaxErr/pairs/noNesting.wacc",
      "test_cases/invalid/syntaxErr/pairs/badLookup01.wacc",
      "test_cases/invalid/syntaxErr/pairs/badLookup02.wacc",
      "test_cases/invalid/syntaxErr/pairs/fstNull.wacc",
      "test_cases/invalid/syntaxErr/pairs/elemOfNonPair.wacc"
    )
    runSyntaxErrorTests(files)
  }

  it should "process syntaxError/if test cases with exit code 100" in {
    val files = List(
      "test_cases/invalid/syntaxErr/if/ifNoelse.wacc",
      "test_cases/invalid/syntaxErr/if/ifNofi.wacc",
      "test_cases/invalid/syntaxErr/if/ifNothen.wacc",
      "test_cases/invalid/syntaxErr/if/ifiErr.wacc"
    )
    runSyntaxErrorTests(files)
  }

  it should "process syntaxError/basic test cases with exit code 100" in {
    val files = List(
      "test_cases/invalid/syntaxErr/basic/bgnErr.wacc",
      "test_cases/invalid/syntaxErr/basic/badEscape.wacc",
      "test_cases/invalid/syntaxErr/basic/beginNoend.wacc",
      "test_cases/invalid/syntaxErr/basic/noBody.wacc",
      "test_cases/invalid/syntaxErr/basic/badComment.wacc",
      "test_cases/invalid/syntaxErr/basic/badComment2.wacc",
      "test_cases/invalid/syntaxErr/basic/multipleBegins.wacc",
      "test_cases/invalid/syntaxErr/basic/skpErr.wacc",
      "test_cases/invalid/syntaxErr/basic/unescapedChar.wacc"
    )
    runSyntaxErrorTests(files)
  }

  it should "process syntaxError/function test cases with exit code 100" in {
    val files = List(
      "test_cases/invalid/syntaxErr/function/badlyNamed.wacc",
      "test_cases/invalid/syntaxErr/function/mutualRecursionNoReturn.wacc",
      "test_cases/invalid/syntaxErr/function/functionConditionalNoReturn.wacc",
      "test_cases/invalid/syntaxErr/function/functionLateDefine.wacc",
      "test_cases/invalid/syntaxErr/function/functionMissingPType.wacc",
      "test_cases/invalid/syntaxErr/function/functionReturnInLoop.wacc",
      "test_cases/invalid/syntaxErr/function/functionMissingCall.wacc",
      "test_cases/invalid/syntaxErr/function/functionNoReturn.wacc",
      "test_cases/invalid/syntaxErr/function/functionScopeDef.wacc",
      "test_cases/invalid/syntaxErr/function/noBodyAfterFuncs.wacc",
      "test_cases/invalid/syntaxErr/function/funcExpr2.wacc",
      "test_cases/invalid/syntaxErr/function/funcExpr.wacc",
      "test_cases/invalid/syntaxErr/function/badlyPlaced.wacc",
      "test_cases/invalid/syntaxErr/function/functionMissingParam.wacc",
      "test_cases/invalid/syntaxErr/function/functionMissingType.wacc",
      "test_cases/invalid/syntaxErr/function/functionEndingNotReturn.wacc",
      "test_cases/invalid/syntaxErr/function/thisIsNotC.wacc"
    )
    runSyntaxErrorTests(files)
  }
  it should "process syntaxError/print test cases with exit code 100" in {
    val files = List(
      "test_cases/invalid/syntaxErr/print/printlnCharArry.wacc"
    )
    runSyntaxErrorTests(files)
  }
  it should "process syntaxError/variables test cases with exit code 100" in {
    val files = List(
      "test_cases/invalid/syntaxErr/variables/badintAssignments1.wacc",
      "test_cases/invalid/syntaxErr/variables/varNoName.wacc",
      "test_cases/invalid/syntaxErr/variables/badintAssignments.wacc",
      "test_cases/invalid/syntaxErr/variables/bigIntAssignment.wacc",
      "test_cases/invalid/syntaxErr/variables/badintAssignments2.wacc"
    )
    runSyntaxErrorTests(files)
  }

  it should "process syntaxError/expressions test cases with exit code 100" in {
    val files = List(
      "test_cases/invalid/syntaxErr/expressions/missingOperand2.wacc",
      "test_cases/invalid/syntaxErr/expressions/printlnConcat.wacc",
      "test_cases/invalid/syntaxErr/expressions/missingOperand1.wacc"
    )
    runSyntaxErrorTests(files)
  }

  it should "process syntaxError/literals test cases with exit code 100" in {
    val files = List(
      "test_cases/invalid/syntaxErr/literals/stringLiteralNoNewlines.wacc",
      "test_cases/invalid/syntaxErr/literals/stringLiteralOnlyAscii.wacc",
      "test_cases/invalid/syntaxErr/literals/charLiteralSingle.wacc"
    )
    runSyntaxErrorTests(files)
  }

  it should "process syntaxError/sequence test cases with exit code 100" in {
    val files = List(
      "test_cases/invalid/syntaxErr/sequence/missingSeq.wacc",
      "test_cases/invalid/syntaxErr/sequence/emptySeq.wacc",
      "test_cases/invalid/syntaxErr/sequence/endSeq.wacc",
      "test_cases/invalid/syntaxErr/sequence/extraSeq.wacc",
      "test_cases/invalid/syntaxErr/sequence/doubleSeq.wacc"
    )
    runSyntaxErrorTests(files)
  }

  it should "process syntaxError/while test cases with exit code 100" in {
    val files = List(
      "test_cases/invalid/syntaxErr/while/whileNodo.wacc",
      "test_cases/invalid/syntaxErr/while/whilErr.wacc",
      "test_cases/invalid/syntaxErr/while/dooErr.wacc",
      "test_cases/invalid/syntaxErr/while/donoErr.wacc",
      "test_cases/invalid/syntaxErr/while/whileNodone.wacc"
    )
    runSyntaxErrorTests(files)
  }

  it should "process semanticError/multiple test cases with exit code 200" in {
    val files = List(
      "test_cases/invalid/semanticErr/multiple/funcMess.wacc",
      "test_cases/invalid/semanticErr/multiple/ifAndWhileErrs.wacc",
      "test_cases/invalid/semanticErr/multiple/obfuscatingReturnsWithWhile.wacc",
      "test_cases/invalid/semanticErr/multiple/multiTypeErrs.wacc",
      "test_cases/invalid/semanticErr/multiple/messyExpr.wacc",
      "test_cases/invalid/semanticErr/multiple/multiCaseSensitivity.wacc"
    )
    runSemanticErrorTests(files)
  }

  it should "process semanticError/array test cases with exit code 200" in {
    val files = List(
      "test_cases/invalid/semanticErr/array/arrayIndexNotInt.wacc",
      "test_cases/invalid/semanticErr/array/noStringIndex.wacc",
      "test_cases/invalid/semanticErr/array/noArrayCovariance.wacc",
      "test_cases/invalid/semanticErr/array/nonMatchingArrays.wacc",
      "test_cases/invalid/semanticErr/array/badIndex.wacc",
      "test_cases/invalid/semanticErr/array/indexUndefIdent.wacc",
      "test_cases/invalid/semanticErr/array/arrayIndexComplexNotInt.wacc",
      "test_cases/invalid/semanticErr/array/mixingTypesInArrays.wacc",
      "test_cases/invalid/semanticErr/array/wrongArrayType.wacc",
      "test_cases/invalid/semanticErr/array/arrayMultipleIndexError.wacc",
      "test_cases/invalid/semanticErr/array/wrongArrayDimension.wacc"
    )
    runSemanticErrorTests(files)
  }

  it should "process semanticError/pairs test cases with exit code 200" in {
    val files = List(
      "test_cases/invalid/semanticErr/pairs/mismatchedPair.wacc",
      "test_cases/invalid/semanticErr/pairs/noPairCovariance.wacc",
      "test_cases/invalid/semanticErr/pairs/nonMatchingPairs.wacc",
      "test_cases/invalid/semanticErr/pairs/freeNonPair.wacc",
      "test_cases/invalid/semanticErr/pairs/wrongTypeInParameterlessPair.wacc",
      "test_cases/invalid/semanticErr/pairs/readUnknown.wacc",
      "test_cases/invalid/semanticErr/pairs/badPairAssign.wacc",
      "test_cases/invalid/semanticErr/pairs/badPairExchange.wacc"
    )
    runSemanticErrorTests(files)
  }

  it should "process semanticError/read test cases with exit code 200" in {
    val files = List(
      "test_cases/invalid/semanticErr/read/readIntoBadSnd.wacc",
      "test_cases/invalid/semanticErr/read/readIntoBadFst.wacc",
      "test_cases/invalid/semanticErr/read/readTypeErr01.wacc"
    )
    runSemanticErrorTests(files)
  }

  it should "process semanticError/if test cases with exit code 200" in {
    val files = List(
      "test_cases/invalid/semanticErr/if/ifIntCondition.wacc"
    )
    runSemanticErrorTests(files)
  }

  it should "process semanticError/IO test cases with exit code 200" in {
    val files = List(
      "test_cases/invalid/semanticErr/IO/readTypeErr.wacc"
    )
    runSemanticErrorTests(files)
  }

  it should "process semanticError/scope test cases with exit code 200" in {
    val files = List(
      "test_cases/invalid/semanticErr/scope/badScopeRedefine.wacc",
      "test_cases/invalid/semanticErr/scope/badParentScope.wacc"
    )
    runSemanticErrorTests(files)
  }

  it should "process semanticError/function test cases with exit code 200" in {
    val files = List(
      "test_cases/invalid/semanticErr/function/callUndefFunction.wacc",
      "test_cases/invalid/semanticErr/function/functionOverArgs.wacc",
      "test_cases/invalid/semanticErr/function/funcVarAccess.wacc",
      "test_cases/invalid/semanticErr/function/functionAssign.wacc",
      "test_cases/invalid/semanticErr/function/functionBadArgUse.wacc",
      "test_cases/invalid/semanticErr/function/functionBadReturn.wacc",
      "test_cases/invalid/semanticErr/function/invalidReturnsBranched.wacc",
      "test_cases/invalid/semanticErr/function/functionUnderArgs.wacc",
      "test_cases/invalid/semanticErr/function/functionRedefine.wacc",
      "test_cases/invalid/semanticErr/function/mismatchingReturns.wacc",
      "test_cases/invalid/semanticErr/function/doubleArgDef.wacc",
      "test_cases/invalid/semanticErr/function/functionBadCall.wacc",
      "test_cases/invalid/semanticErr/function/functionBadParam.wacc",
      "test_cases/invalid/semanticErr/function/functionSwapArgs.wacc"
    )
    runSemanticErrorTests(files)
  }

  it should "process semanticError/print test cases with exit code 200" in {
    val files = List(
      "test_cases/invalid/semanticErr/print/printTypeErr01.wacc"
    )
    runSemanticErrorTests(files)
  }

  it should "process semanticError/variables test cases with exit code 200" in {
    val files = List(
      "test_cases/invalid/semanticErr/variables/basicTypeErr12.wacc",
      "test_cases/invalid/semanticErr/variables/basicTypeErr04.wacc",
      "test_cases/invalid/semanticErr/variables/doubleDeclare.wacc",
      "test_cases/invalid/semanticErr/variables/basicTypeErr08.wacc",
      "test_cases/invalid/semanticErr/variables/basicTypeErr09.wacc",
      "test_cases/invalid/semanticErr/variables/basicTypeErr05.wacc",
      "test_cases/invalid/semanticErr/variables/basicTypeErr02.wacc",
      "test_cases/invalid/semanticErr/variables/basicTypeErr03.wacc",
      "test_cases/invalid/semanticErr/variables/undeclaredScopeVar.wacc",
      "test_cases/invalid/semanticErr/variables/undeclaredVar.wacc",
      "test_cases/invalid/semanticErr/variables/undeclaredVarAccess.wacc",
      "test_cases/invalid/semanticErr/variables/basicTypeErr01.wacc",
      "test_cases/invalid/semanticErr/variables/basicTypeErr06.wacc",
      "test_cases/invalid/semanticErr/variables/basicTypeErr10.wacc",
      "test_cases/invalid/semanticErr/variables/caseMatters.wacc",
      "test_cases/invalid/semanticErr/variables/basicTypeErr11.wacc",
      "test_cases/invalid/semanticErr/variables/basicTypeErr07.wacc"
    )
    runSemanticErrorTests(files)
  }

  it should "process semanticError/expressions test cases with exit code 200" in {
    val files = List(
      "test_cases/invalid/semanticErr/expressions/mixedOpTypeErr.wacc",
      "test_cases/invalid/semanticErr/expressions/boolOpTypeErr.wacc",
      "test_cases/invalid/semanticErr/expressions/intOpTypeErr.wacc",
      "test_cases/invalid/semanticErr/expressions/exprTypeErr.wacc",
      "test_cases/invalid/semanticErr/expressions/lessPairExpr.wacc",
      "test_cases/invalid/semanticErr/expressions/moreArrExpr.wacc",
      "test_cases/invalid/semanticErr/expressions/stringElemErr.wacc"
    )
    runSemanticErrorTests(files)
  }

  it should "process semanticError/exit test cases with exit code 200" in {
    val files = List(
      "test_cases/invalid/semanticErr/exit/badCharExit.wacc",
      "test_cases/invalid/semanticErr/exit/exitNonInt.wacc",
      "test_cases/invalid/semanticErr/exit/globalReturn.wacc",
      "test_cases/invalid/semanticErr/exit/returnsInMain.wacc"
    )
    runSemanticErrorTests(files)
  }

  it should "process semanticError/while test cases with exit code 200" in {
    val files = List(
      "test_cases/invalid/semanticErr/while/truErr.wacc",
      "test_cases/invalid/semanticErr/while/falsErr.wacc",
      "test_cases/invalid/semanticErr/while/whileIntCondition.wacc"
    )
    runSemanticErrorTests(files)
  }

  private def getFile(fileName: String): String = {
    var file = new java.io.File(fileName)
    if (file.exists()) file.toPath().toAbsolutePath().toString()
    else {
      file = new java.io.File("../" + fileName)
      if (file.exists()) file.toPath().toAbsolutePath().toString()
      else throw new IllegalArgumentException("The provided path is not a valid file.")
    }
  }

  private def getFiles(folderName: String): List[String] = {
    var folder = new java.io.File(folderName)
    if (!folder.exists() || !folder.isDirectory()) {
      folder = new java.io.File("../" + folderName)
      if (!folder.exists() || !folder.isDirectory()) {
        throw new IllegalArgumentException("The provided path is not a valid directory.")
      }
    }

    val stream = Files.walk(folder.toPath())
    try {
      stream.iterator().asScala.filter(Files.isRegularFile(_)).toList.map(path => path.toString())
    } finally {
      stream.close()
    }
  }

  private def runTests(files: List[String], expectedExitCode: Int) = {
    val result: List[Int] = files.map(filename => {
      val res = wacc.Main.runFile(getFile(filename))
      res._1 match {
        case None => 
        case Some(value) => PrettyPrint.prettyPrint(value)
      }
      val exitCode = res._2
      if (exitCode == expectedExitCode) {
        // println(s"$ANSI_GREEN $filename successful - exited $exitCode $ANSI_RESET")
      } else {
        println(
          s"$ANSI_RED $filename failed - expected $expectedExitCode, actual exit $exitCode $ANSI_RESET"
        )
      }
      exitCode
    })
    result.foreach { case exitCode => { exitCode shouldBe expectedExitCode } }
  }

  private def runValidTests(files: List[String])         = runTests(files, SUCCESS_CODE)
  private def runSyntaxErrorTests(files: List[String])   = runTests(files, SYNTAX_ERROR_CODE)
  private def runSemanticErrorTests(files: List[String]) = runTests(files, SEMANTIC_ERROR_CODE)
}
