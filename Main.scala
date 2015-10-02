/**
 * Created by mafu on 26.09.2015.
 */

/*
How to simply sum values in a Map in Scala
  Here is a code snippet that sums up the values in a map in Scala:
  m.foldLeft(0)(_+_._2)

It may look cryptic at first but once you get the Scala feel, you will appreciate its conciseness and elegance. Here below
is the explanation:
  m stands for a Map, foldLeft is a method that takes an initial value (0 here since sum is 0 at the beginning) and feeds
  it to its next iteration on this map entry. Note that the first _ stands for the result of previous iteration result.
  And _+_._2 means:
  take previous result and add it to the next map entry's value, which _._2 stands for. In Scala, Map values can be
  accessed as Tuples. _1 is the key and _2 is the value. Here is a working example:
*/

import java.io.{IOException, FileInputStream, File}
import scala.math
import sun.security.pkcs11.Secmod.DbMode

import scala.collection

class Testing{
  val m = scala.collection.mutable.Map("x" -> 24, "y" -> 25, "z" -> -3)
  def default = (s: String) => 0

  def printAndUpdateMap(): Unit ={
    //val source : String = scala.io.Source.fromFile(file,"latin1").getLines.mkString
    println(m)
    m.update("a", 17)
    println(m)
    m.update("x", 25)
    println(m)
    val n = m.applyOrElse("x", default)
    println(n)
  }
}

class DocElement(path : File, doctype : String){
  //Default constructor
  val pathDirectory : File = path //can be accessed, but cannot be changed
  val documentTyp : String = doctype
  var numberOfTokens : Double = 0
  var tokenList : scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()
  var normalizedTokenList : collection.Map[String, Double] = collection.Map()

  //Auxiliary constructor
  def this(path : File) {
    //Call default constructor
    this(path, "")

    //Get all tokens with counts as a Map
    tokenList = aggregateTokens(path)

    //Normalize tokenList
    numberOfTokens = tokenList.foldLeft(0)(_+_._2)
    normalizedTokenList = tokenList.mapValues(v => v.toDouble/numberOfTokens)
  }

  def defaultInt = (s: String) => 0.toInt
  def defaultDouble = (s: String) => 0.toDouble

  def aggregateTokens(file: File) : scala.collection.mutable.Map[String, Int] = {
    val tL : scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()

    val sourceIter : Iterator[Char] = scala.io.Source.fromFile(file,"latin1").iter
    while(sourceIter.hasNext){
      var tempToken : String = ""
      while(sourceIter.hasNext && tempToken.length < 3){
        tempToken += sourceIter.next()
      }
      val nextToken : String = tempToken.replaceAll( "[.,?﻿\"!-_;\\t\\n\\r\\f]" , " " ).toLowerCase()
      tL.update( nextToken, tL.applyOrElse(nextToken, defaultInt)+1 )
    }
    return tL
  }
}

class DocCollection(path : File, lang : String, maxNumberOfDocs : Int, doctype : String) extends DocElement(path,doctype){
  //Default constructor
  val language : String = lang
  val maxNumberOfDocuments : Int = maxNumberOfDocs
  private var fileList : List[File] = List()

  //Auxiliary constructor
  def this(path : File, lang : String) {
    //Call default constructor
    this(path, lang, 0, "")

    //Get a list with all files
    fileList = getAllFiles(path)

    //Get all tokens with counts as a Map
    for(file <- fileList) {
      val tL : scala.collection.mutable.Map[String, Int] = aggregateTokens(file)
      for(nextToken <- tL){
        tokenList.update( nextToken._1, tokenList.applyOrElse(nextToken._1, defaultInt)+nextToken._2 )
      }
    }

    //Normalize tokenList
    numberOfTokens = tokenList.foldLeft(0)(_+_._2)
    normalizedTokenList = tokenList.mapValues(v => v.toDouble/numberOfTokens)
  }

  //Get all files of a folder
  def getAllFiles(path : File) : List[File] = {
    val parts = path.listFiles.toList.partition(_.isDirectory)
    parts._2 ::: parts._1.flatMap(getAllFiles)
  }

  //Print document colletion
  def printDocCollection() : Unit = {
    fileList.foreach(f => println(f))
  }

  //Get size of the document collection
  def getNumberOfDocuments() : Int = {
    return fileList.size
  }

  def calculateMaxLikelihood(tokenL : collection.Map[String, Int] ) : Double = {
    var num : Double = 0
    var ml : Double = 0
    for(t <- tokenL){
      num = normalizedTokenList.applyOrElse(t._1, defaultDouble) * t._2
      if (num == 0) ml += 0 else ml += scala.math.log(num)
    }
    return -ml
  }
}

object Main {
  def main(args: Array[String]) {
    println("Exercise 02 - Language Recognition")

    //0. Variables
    println("***0. Variables***")
    val pathTrainingEN : File = new File("data/training/EN")
    val pathTrainingDE : File = new File("data/training/DE")

    val pathTesting1 : File = new File("data/testing/pg2146_DE.txt")
    val pathTesting2 : File = new File("data/testing/pg4300_EN.txt")


    //1. Training
    println("***1. Training***")
    println("Read all training Documents")
    val trainingDocEN = new DocCollection(pathTrainingEN, "EN")
    val trainingDocDE = new DocCollection(pathTrainingDE, "DE")

    println( "Number of training docs EN: " + trainingDocEN.getNumberOfDocuments() )
    println( "Number of training docs DE: " + trainingDocDE.getNumberOfDocuments() )

    //2. Testing and maximum likelihood
    println("***2. Testing and maximum likelihood***")
    println("Read test Document")

    val testDocDE1 = new DocElement( pathTesting1 )
    println("pg2146_DE.txt")
    println("Maximum likelihood DE = " + trainingDocDE.calculateMaxLikelihood(testDocDE1.tokenList ) )
    println("Maximum likelihood EN = " + trainingDocEN.calculateMaxLikelihood(testDocDE1.tokenList ) )

    val testDocDE2 = new DocElement( pathTesting2 )
    println("pg4300_EN.txt")
    println("Maximum likelihood DE = " + trainingDocDE.calculateMaxLikelihood(testDocDE2.tokenList ) )
    println("Maximum likelihood EN = " + trainingDocEN.calculateMaxLikelihood(testDocDE2.tokenList ) )
  }
}
