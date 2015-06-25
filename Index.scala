package search
import scala.xml.Node
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LinkedList
import java.io._
import java.net.URLEncoder
import java.math.BigDecimal
import org.xml.sax.SAXParseException

/**Class that deals with sorting all the words in the passed in corpus
 * and their respective frequencies in the pages in the corpus
 * and understanding all the relative value(iceRank) of each of the pages
 */
class Index(corpus: String, indexFile: String, iceRankFile: String) {
  private final val dampening : Double = .85
  private val (idHash : HashMap[Int, Page], titleHash : HashMap[String, Int]) = parseXML
  private val wordScores : HashMap[String, LinkedList[(Int, Double)]] = scoreHash
  private val iceRanks : HashMap[Int, Double] = generateRank
  
  
  
  
  
  /**After having constructed all the necessary HashMaps that define the corpus 
   * and calculating all the necessary values to be used for the query
   * (all called in the constructor) this method writes all these values to the
   * passed in files
   */
  private def createFile = {
    var counter = 0
    try {
    val fw: BufferedWriter = new BufferedWriter(new FileWriter(indexFile))
    var wordValueString = ""
    for ((word, ll) <- wordScores) {
      wordValueString = ""
      val inDocFreq = Math.log(idHash.size / (1.0 + ll.length))
      wordValueString += word + "\t" + inDocFreq
      for ((id, docFreq) <- ll) {
        wordValueString += "$"+id+ "&" + docFreq
      }
      counter = counter + 1
      fw.write(wordValueString+"\n")
    }
    fw.close
    
    counter = 0
    val w: BufferedWriter = new BufferedWriter(new FileWriter(iceRankFile))
    for ((id, rank) <- iceRanks) {
      w.write(id + "\t" + rank + "\n")
      counter = counter + 1
    }
    w.write("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
    counter = 0
    for((pageName, id) <- titleHash) {
      w.write(pageName + "\t" + id + "\n")
      counter = counter + 1
    }
    w.close
  	} catch {
  	  case a : IOException => println("There is an error in the file creation")
  	}
  }
  
  
  
  
  
  
  /**Uses the corpus assigned to this Index, and constructs a HashMap
   * from each page's id to it's instance of a Page (our class), as well
   * as another HashMap from the name of the name of the page to its id
   * 
   * @ return tuple of Hashmaps, one from id to page, another from
   * page name to id
   */
  private def parseXML: (HashMap[Int, Page], HashMap[String, Int])  = {
    val root: Node = xml.XML.loadFile(corpus)
    val xmlPages: Seq[Node] = root.child.filter(x => x.label.contains("page"))
    val idHash : HashMap[Int, Page] = new HashMap()
    val titleHash : HashMap[String, Int] = new HashMap
    for (page <- xmlPages) {
      val id: Int = Integer.parseInt((page \ "id").text.trim)
      val title: String = (page \ "title").text.trim
      val text: String = (page \ "text").text.trim
      val p: Page = new Page(id, title, text)
      idHash.put(id, p)
      titleHash.put(title, id)
    }
    return (idHash, titleHash)
  }
  
  
  
  
  
  
  /**For every word in the corpus constructs a HashMap of a word to a Linked list that
   * contains the ids of all the pages that contain this word, as well as that
   * word's value for that page (frequency/ Euclidean's algorithm calculation)
   * 
   * @ return the hashmap that links a word to all the pages that contain it
   * and their respective scores
   */
  private def scoreHash: HashMap[String, LinkedList[(Int, Double)]] = {
    val wordsInAll : HashMap[String, LinkedList[(Int, Double)]] = new HashMap()
    for ((id, page) <- idHash){
      for ((word, freq) <- page.textHashed){
        val v : Option[LinkedList[(Int, Double)]] = wordsInAll.get(word)
        if(page.euclid != 0){
        v match {
        	case None => wordsInAll.put(word, LinkedList((id, freq/ page.euclid)))
        	case Some(linkedList) => wordsInAll.update(word, (id, freq/ page.euclid)+:linkedList)
      	}
        }
      }
    }
    return wordsInAll
  }

  
  
  
  
  
  /**Generates an iceRanks for all the pages in the corpus
   * 
   * @ return - a HashMap where each page is identified with an id, that 
   * matches to a specific iceRank value, that is calculated in this method
   */
  private def generateRank : HashMap[Int, Double] = {
    val currentRank : HashMap[Int, Double]= new HashMap()
    val toAdd : HashMap[Int, Double]= new HashMap()
    for ((id, _) <- idHash) {
      currentRank.put(id, 1.0/idHash.size)
      toAdd.put(id, 0)
    } 
    
    var counter = 0
    var keepGoing : Double = 1
    while (keepGoing > 1.0 / math.pow(idHash.size, 6.0) && counter < 1000) { 
      for ((id, s) <- currentRank){
        idHash.get(id) match {
          case None => print("This isn't a good corpus1")
          case Some(page) => 
            for (link <- page.linksArray){
              titleHash.get(link) match {
                case None => 
                case Some(idOfLink) => 
                  toAdd.get(idOfLink) match {
            		case None =>
            		case Some(score) => toAdd.update(idOfLink, score + s/page.linksArray.size)
            	}
              }
            }
        }
      }
      
      keepGoing = 0
      for ((id, n) <- toAdd){
        val newValue = (1 - dampening) / idHash.size + dampening * (n)
        keepGoing = math.abs(newValue - currentRank.getOrElse(id, 0.0)) + keepGoing
        currentRank.update(id, newValue)
        toAdd.update(id, 0)
      }
      counter += 1
    }
    return currentRank
  }
}






object Index {
  /**Main method, takes in the arguments that indicated the files
   * the index class should write to.
   * 
   * @param args - array of string of commands that indicate where 
   * to save the knowledge about this corpus.
   */
  def main(args: Array[String]) {
    
    if (args.length == 3){
      try {
      val index : Index = new Index(args(0), args(1), args(2))
      index.createFile
      } catch {
        case e : NumberFormatException => println("Bad input file")
        case e1 : FileNotFoundException => println("Bad input file")
        case e2 : IllegalArgumentException => println("Bad input file")
        case e3 : SAXParseException => println("Bad input file")
      }
    } else {
      println("Hey, try the right number of arguments (3) next time.")
    }
  }
}