package machine

import org.junit.Test
import org.junit.Assert._

import main.AnalysePhraseD
import main.BaseDeDonnees
import main.AnalyserBDD

class TestAnalysePhraseD {
  
  val bdd : BaseDeDonnees = new AnalyserBDD("doc/DonneesTest.txt")
  val analyser: AnalysePhraseD = new AnalysePhraseD(bdd)
  // tests

  @Test
  def test1_1 {
    assertEquals(Set(("Mairie","adM")), analyser.analyse("je veux la mairie"))
    assertEquals(Set(("Mairie","adM")), analyser.analyse("je veux l'Hôtel de ville"))

    assertEquals( Set(("Boucher", "adB")), analyser.analyse("je veux le boucher"))
    assertEquals(Set(("Boucher", "adB")), analyser.analyse("Cherche moi la boucherie"))
    
    assertEquals(Set(("Parc Michelle", "adP")), analyser.analyse("Ou est le parc le plus proche"))
    assertEquals(Set(("Parc Michelle", "adP")), analyser.analyse("Comment aller au parc michelle"))

    assertEquals(Set(("Forêt du Loup", "adF")), analyser.analyse("Je veux me balader dans la Foret"))
    assertEquals(Set(("Forêt du Loup", "adF")), analyser.analyse("je veux bruler toute la Forêt"))
  }

  @Test
  def test_1_2 {
    assertNotEquals("Je veux me balader dans la Foret", "L'adresse de la Foret est : adF")
  }

  

  @Test
  def test_rawAnalyse {
    val expected: List[(String, String)] = List(
      ("Mairie", "adM"),
      ("Mairie", "adM"),
      ("Boucher", "adB"),
      ("Boucher", "adB"),
      ("Parc Michelle", "adP"),
      ("Parc Michelle", "adP"),
      ("Forêt du Loup", "adF"),
      ("Forêt du Loup", "adF"))
      
      val requests : List[String] = List("Mairie", "Hôtel de ville", "Boucher", "Boucherie", "Parc Michelle", "Parc", "Foret du Loup", "Foret")
      (requests zip expected).map({ case (request,exp) => assertEquals(List(exp), analyser.rawAnalyse(request)) })
  }
  
}