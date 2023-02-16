package main

import java.text.Normalizer
import java.util.regex.Pattern;


object ToleranceErreur extends CorrectionErreur {

   /**
   * Corrige un mot avec une tolerance de 
   * 1 caractere de difference ou 1 caractere en plus ou 1 caractere en moins
   * 
   * @param prend un string
   * @param dict, un dictionnaire de mot
   * @return le mot corrigé si besoin et si il y a au plus une erreur
   */
  def corrige(cle:String, dict:Set[String]):String = {
    val quest : String = cle.trim
    val words : List[String] = quest.split("\\W+").toList // Split tout les ensemble de non mots
    if (words.size == 0) ""
    else if (words.size == 1) correctWord(words.head, dict)
    else {
      val separator : List[String] = quest.split("\\w+").toList //Split tout les mots
      val correctWords : List[String] = words.map(s => correctWord(s, dict))
      
      (separator zip correctWords).flatten({ case (a,b) => List(a,b) }).mkString
    }
  }
  
  /**
   * Corrige un mot a partir d'un dictionnaire
   * @param s, ce mot
   * @param dict, le dictionnaire
   */
  def correctWord(s : String, dict:Set[String]): String = {
    dict.find(cle => distLevenshtein(stripAccent(cle.toLowerCase), cle.length, stripAccent(s.toLowerCase), s.length)<=1) match {
      case None => s
      case Some(word) => word
    }
  }
 
  /**
  * Remplace tout les caractere accentue par des caractere non accentue
  * @param s, une chaine de caractere
  * @return cette chaine sans caractere accentue
  */
 def stripAccent(s:String):String = {
    val strTemp = Normalizer.normalize(s, Normalizer.Form.NFD);
        val pattern = Pattern.compile("\\p{InCombiningDiacriticalMarks}+");
        pattern.matcher(strTemp).replaceAll("");
 }
  /**
   * @param prend un mot n 1
   * @param prend un mot n 2
   * @param prend la taille du mot n 1
   * @param prend la taille du mot n 2
   * @ return le nombre de différences entre le mot 1 et le mot 2
   */
  
   def distLevenshtein(mot1:String,taille1:Int,mot2:String,taille2:Int):Int={
    
    var cout:Int=0;
  
    
    if(taille1==0){
      return taille2
    }
    if(taille2==0){
      return taille1
    }
    if (mot1.charAt(taille1-1) == mot2.charAt(taille2-1)) {
      cout = 0;
    }
    else cout = 1;
    return min(distLevenshtein(mot1, taille1 - 1, mot2, taille2    ) + 1,
                 distLevenshtein(mot1, taille1 , mot2, taille2 - 1) + 1,
                 distLevenshtein(mot1, taille1 - 1, mot2, taille2 - 1) + cout);
  }
  
  /**
   * @param un int
   * @param un int
   * @param un int
   * @ return renvoie le plus petit des 3 sinon retourne 0
   */
   
  def min(i:Int,j:Int,k:Int):Int={
   if(i<=j && i<=k){
     return i
   }
   if(j<=i && j<=k){
     return j
   }
   if(k<=i && k<=j){
     return k
   } 
   return 0
  }
}