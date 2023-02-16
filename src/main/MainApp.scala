package main

import scala.swing._
import scala.swing.MainFrame

object MainApp extends SimpleSwingApplication {
  
  def top : Interface = new GUI(response)

  var bdd : BaseDeDonnees = new AnalyserBDD("doc/DonneesInitiales.txt")
  var annalysePhrase : AnalysePhrase = new AnalysePhraseD(bdd)

//  def displayAnswers(l : List[String]): Unit = {
//    l match {
//      case Nil => println()
//      case e :: end => println("\t" + "—"*(e.size + 2)); println("\t| " + e + " |"); println("\t" + "—"*(e.size + 2)); displayAnswers(end)
//    }
//  }
  
  def response(s: String): List[String] = {
    val sayBonjour: Boolean = "bonjour[s]?".r.findFirstIn(s.toLowerCase) != None
    val request = "bonjour[s]?".r.replaceAllIn(s.toLowerCase.trim, "")
    
    val answers : List[String] = getAnswers(annalysePhrase.analyse(request).toList)
    
    val bonjour: List[String] = if (sayBonjour) List("Bonjour") else Nil
    
    if (answers.isEmpty && !request.isEmpty()) bonjour ++ List("Je ne comprends pas votre demande")
    else bonjour ++ answers

  }
  
  /**
   * Creer des reponse a afficher a partir de couple (lieux, adresse) 
   * 
   * @param une liste de (lieux, adresse)
   * @return une liste de reponse formuler "L'adresse de [lieux] est : [adresse]"
   */
  def getAnswers(l : List[(String,String)]): List[String] = {          // Prepare the final String to return
    // Collecte les élements si vérifié et leurs applique une transformation
    l.collect({case (name, adress) if (!adress.isEmpty)=> s"L'adresse de $name est : $adress"}) 
  }
  
  def changeBDD(file: String) {
    bdd = new AnalyserBDD(file)
    annalysePhrase = new AnalysePhraseD(bdd)
  }
}