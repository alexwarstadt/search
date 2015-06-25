package search
import scala.collection.mutable.HashMap
import java.net._

/**Stores information about a specific words's value in a current page
 * the unparsed information is passed in as information, and then the 
 * Class stores the inverse document frequency for this word across all the pages
 * and then creates an array of tuples of page ids (that have the word
 * that defines this Info class) and the word's value in that page. 
 * 
 * @param information - the unparsed information about the pages 
 * that contain the word that is assigned the Info instance
 */
class Info(information: String) {
  var idf: Double = 0.0
  val docScores: Array[(Int, Double)] = this.parseInfo
  
  
  
  
  
  /**Takes the passed in information string and parses all
   * of its values - inverse document frequency, page ids, and values
   * in each page, and sets the idf value in the page, and 
   * creates the Array of tuples (page ids -> word value in page)
   * 
   * @return - the Array of tuples (page ids -> word value in page)
   * for the word that is assigned this Info instance
   */
  def parseInfo(): Array[(Int, Double)] = {
    val arrayOfDocs = information.split("[$]")
    val tempDocScores : Array[(Int, Double)] = new Array(arrayOfDocs.size -1)
    idf = arrayOfDocs(0).toDouble
    var eachPageInfo : Array[String] = new Array[String](2)
    for (pos <- 1 until arrayOfDocs.size) {
      eachPageInfo = arrayOfDocs(pos).split("[&]")
      tempDocScores(pos-1) = (eachPageInfo(0).toInt, eachPageInfo(1).toDouble)
    }
    return tempDocScores
  }
  
  
  
  
  
  /**Inserts all of this Info class's docScore tuples into the passed in 
   * Hash, taking into account he inverse document frequency.
   * This is used for multiword query, as the hashmap must be built with
   * all the scores for every page for this query
   * 
   * @param hash - HashMap where you want to insert all the docScores
   * of this Info into
   */
  def insertScores(hash: HashMap[Int, Double]) = {
    for ((id, d) <- docScores) {
      hash.get(id) match {
        case None => hash.put(id, idf*d)
        case Some(n) => hash.update(id, n+ idf*d)
      }
    }
  }
  

}