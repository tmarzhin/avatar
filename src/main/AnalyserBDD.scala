package main

import scala.io.Source


class AnalyserBDD(file: String) extends BaseDeDonnees {
  
  var bdd:Map[String, String] = Map()
  var donnees:Map[String, String] = Map()
   
  var lines = Source.fromFile(file).getLines   
  while(lines.hasNext){
      val tab = lines.next().split("\\s*;\\s*").toList
      // Creer une liste de tuple associant tout les élément au premier sauf pour le dernier
      val names: List[(String, String)] = tab.take(tab.size - 1) zip List.fill(tab.size-1)(tab.head)
      
      // Associe ces couple dans la map donnees
      donnees ++= names.toMap
      
      // Creer une list de tuple associant les n-1 premier element au dernier element
      val values: List[(String, String)] = tab.take(tab.size - 1) zip List.fill(tab.size-1)(tab.last)
      
      // Associe les nom de lieux a l'adresse
      bdd ++= values.toMap
  }
  
  
  private def reponseAdresse(key:String):String = {
    donnees.getOrElse(key, "");
  }
   
  /**
   * Recupère tous les mots clés de la base de données
   * @return les mots clés de la BDD
   */
   def getDico() :Set[String] = {
     bdd.keySet
   }
   
   
   /**
   * Recupère le nom principal du lieu
   * @param e mot clé recherché
   * @return str le nom principal de ce lieu
   * 
   */
  private def getPrimaryName(e:String) : String = {
    donnees.getOrElse(e, "NotFound")
  }
  
  /**
   * Recherche un mot clé dans la base données et renvoie le lieu et son adresse
   */
  def searchInDict(s : String): (String, String) = {
    searchInDictRec(s, getDico().toList)
  }
  
  private def searchInDictRec(s : String, dict: List[String]): (String, String) = {
    if(s.isEmpty()) ("","")
    else
    dict match {
      case Nil => ("","")
      case e :: end => if (ToleranceErreur.stripAccent(s.toLowerCase).contains(ToleranceErreur.stripAccent(e.toLowerCase))) {
        (getPrimaryName(e), bdd.getOrElse(e,"")) 
      } else {
          searchInDictRec(s,end)
      }
    }
  }
  
  
  def searchList(l:List[String]):String = {
    var str ="";
    l match {
      case Nil => ""

      case List(a) => bdd.getOrElse(a, "Je ne comprends pas votre demande")
      case e::end => 
        for(x <- bdd.keys){
          if(x.contains(e)){
            str = bdd.getOrElse(x, "Je ne comprends pas votre demande")+"\n" + searchList(end);
          }
        }
        str
    }
  }
  
}