package search
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

/**Used by the Index class to define each child of the xml node
 * Parses all the text and links of the page 
 * 
 * @param id - id node from xml
 * @param title - title node from xml of the page
 * @param text - text node from the xml of the page
 */
class Page(id: Int, title: String, text: String) {
  val textHashed : HashMap[String, Int] = this.wordCounts
  val linksArray : ArrayBuffer[String]= this.getLinks
  val euclid : Double = this.euclidCalc
  
  
  
  
  /**Goes through the whole string of words and either inserts them into
   * the HashMap that maps from words to frequency,
   * if the word is not there yet it add it to the HashMap, or 
   * increments the word's frequency in the HashMap
   * 
   * @return Hashmap of all the words in the passed in text to their
   * frequency in this current page
   */
  private def wordCounts: HashMap[String, Int] = {
    val unstemmed: Array[String] = text.split("[^a-zA-Z0-9]")
    val stemmed: Array[String] = PorterStemmer.stemArray(unstemmed)
    val wordFreq: HashMap[String, Int] = new HashMap
    for (w <- stemmed){
      if (!w.equals("")){
        val word = w.toLowerCase
        val v : Option[Int] = wordFreq.get(word)
        v match {
          case None => wordFreq.put(word, 1)
          case Some(n) => wordFreq.update(word, n + 1)
        }
      }
    }
    return wordFreq
  }
  
  
  
  /**Calculates the Euclidian normalization for this specific page
   * based on the frequencies of every word
   * 
   * @return the Euclidian normalization for this page
   */
  private def euclidCalc : Double = {
    var totalSquares = 0.0
    
    for ((_, freq) <- textHashed){
      totalSquares += freq^2
    }
    
    return Math.sqrt(totalSquares)
  }
  
  
  
  
  /**Matches all the text to a specific regex, to find alll the
   * links in the passed in text, and add them to an ArrayBuffer
   * 
   * @return an ArrayBuffer of all this page's links
   */

  private def getLinks: ArrayBuffer[String] = {
    val links = new ArrayBuffer[String]    
    val BasicLinkPattern = "\\[\\[(.*)\\]\\]".r
    val LinkPattern = "([\\s\\S]*)\\[\\[(.*)\\]\\]([\\s\\S]*)".r
    var a: String = this.text
    while(!a.equals("")){
       a match {
        case LinkPattern(x,y,z) => links += y
          a = x
        case BasicLinkPattern(x) => links += x
          a = ""
        case _ =>
          a = ""
      }
    }
    return links
  }
}