package main

class AnalysePhraseD(bdd : BaseDeDonnees) extends AnalysePhrase {
  val dict : Set[String] = bdd.getDico();
  
  /**
   * Retourne une liste de reponse a une requete
   *  - Analyse la phrase avec la base de donnees pour trouver des reponses
   *  - Si trouve un ou plusieurs resulat retourne les reponses
   *  - Sinon tente de corriger la phrase et re analyse
   *  
   *  @param cle, la requete
   *  @return la liste de reponse
   */
  def analyse(quest:String):Set[(String,String)] = {
    val firstAnalyse: List[(String,String)] = rawAnalyse(quest)
    if (!firstAnalyse.isEmpty) firstAnalyse.toSet
    else {
      val correctedStr: String = ToleranceErreur.corrige(quest, dict.flatMap(_.split("\\W+"))) // Split les cle en mot
      //println("Corrected : " + correctedStr + "/")
      rawAnalyse(correctedStr).toSet
    }
  }
  
  /**
   * Prend une String et analyse le contenu a l'aide du dictionnaire
   * Retourne les couples (destination, adresse)
   * 	- Decoupe la String en fonction de "et", de "ou" et de ","
   * 	- Analyse chaque partie decoupe pour trouver une correspondance
   * 		dans la base de donnees
   * @param s, la string a analyser
   * @return les couple avec le nom du lieux et son adresse
   */
  def rawAnalyse(s : String): List[(String,String)] = {
    val lWords: List[String] = s.split(",| ou | et ").toList
    getAdress(lWords)
  }

  /**
   * Recherche dans la base de donne l'adresse des lieux inscrit dans la liste
   * 
   * @param l, la liste des lieux
   * @return le couple (lieux, adresse)
   */
  def getAdress(l : List[String]): List[(String,String)] = {
    l match {
      case Nil => Nil
      case e :: end => val r : (String,String) = bdd.searchInDict(e.trim);
        if (r._2.isEmpty) getAdress(end) 
        else r :: getAdress(end)
    }
  }
}