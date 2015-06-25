package search
import java.io._
import scala.collection.mutable.HashMap
import scala.collection.mutable.PriorityQueue

/**After running Indexing on an xml corpus, query actually looks up
 * the value of a word in the corpus using the txt files created in Index
 * and then incorporates each page's iceRank score
 * 
 * @param iceRank - boolean, whether to run the query with iceRank or not
 * @param titlesFile - file name string that indicates the name of the file
 * where the iceRank for each page as well as the page to id hash is stored
 * @param indexFile - the file name string where each words is next to its information
 * about its presence in the corpus resides
 */
class Query (iceRank: Boolean, titlesFile: String, indexFile: String){
  val (titles, iceRanks) : (HashMap[Int, String], HashMap[Int, Double]) = createTitle
  val index : HashMap[String, Info] = createIndex
  
  
  
  
  /**Creates an id->page name hash, and a id->iceRank HashMap form the 
   * titlesFile file
   * 
   * @return a tuple consisting of the id-> page HashMap and the 
   * id -> iceRank HashMap
   */
  private def createTitle : (HashMap[Int, String], HashMap[Int, Double]) = {
    val tempTitles: HashMap[Int, String] = new HashMap()
    val tempIceRanks: HashMap[Int, Double] = new HashMap()
    val r : BufferedReader = new BufferedReader(new FileReader(titlesFile))
    var line: String = r.readLine()
    while(line != null && line != "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%") {
      val kv: Array[String] = line.split("\t")
      if (kv.size != 2){
        throw new IllegalArgumentException
      }
      tempIceRanks.put(Integer.parseInt(kv(0)), kv(1).toDouble)					
      line = r.readLine()
    }
    line = r.readLine()
    while (line != null){
      val kv: Array[String] = line.split("\t")
      if (kv.size != 2){
        throw new IllegalArgumentException
      }
      tempTitles.put(Integer.parseInt(kv(1)), kv(0))					
      line = r.readLine()
    }
    return (tempTitles, tempIceRanks)
  }
  
  
  
  
  /**Creates a HashMap of word -> an instance of Info 
   * (which stores the inverse word frequency, and all the pages
   * that contain the word and their score, based on the preset calculations)
   * it creates it all from the input indexFile
   * 
   * @return a HashMap of words -> Info
   */
  private def createIndex : HashMap[String, Info] = {
    val r : BufferedReader = new BufferedReader(new FileReader(indexFile))
    var line: String = r.readLine()
    val tempIndex: HashMap[String, Info] = new HashMap()
    while(line != null) {
      val kv: Array[String] = line.split("\t")
      val v: Info = new Info(kv(1))
      tempIndex.put(kv(0), v)
      line = r.readLine()
    }
    return tempIndex
  }
  
  
  
  
  /**Method that is called to actually start the querying process, calls all
   * the necessary methods to get input and look it up
   */
  private def runQuery() = {  
    var running: Boolean = true
    while(running){
      var input: String = this.getInput
      if (input.equals(":quit")){
        running = false
      } else {
        this.lookup(input)
      }
    }
  }
  
  
  
  
  /**Gets the input from the user of what word they want to be looked up in
   * our system
   * 
   * @return a trimmed String of the user's query
   */
  private def getInput: String = {
    val r : BufferedReader = new BufferedReader(new InputStreamReader (System.in))
    println("Enter your query")
    var input = r.readLine()
    if (input == null){
      println("Invalid input. Try again!")
      input = getInput
    }
    return input.trim
  }
  
  
  
  
  /**Stems the words passed in by the user, looks them up in the necessary HashMap
   * If the Query has been instantiated with iceRank then calls iceRank method
   * and eventually calls topTen to print out the top 10 searches
   * 
   * @param input - the String that the user wants to query
   */
  private def lookup(input: String) = {
    val words: Array[String] = PorterStemmer.stemArray(input.split("[\\s\\W]"))
    val arrayArrays : Array[Array[Info]] = new Array(words.size)
    var epicScores: HashMap[Int, Double] = new HashMap
    for (w <- words){
      this.index.get(w.toLowerCase) match {
        case None => 
        case Some(info) => info.insertScores(epicScores)
      }
    }
    if (this.iceRank) {
      epicScores = iceRank(epicScores)
    }
    topTen(epicScores)
  }
  
  
  
  
  /**Takes in a HashMap of each page that has one of the words in the query present
   * so the HashMap maps ids of pages -> their score for the query, and the method
   * changes incorporates the iceRank of each page into their scores
   * and then returns the new HashMap
   * 
   * @param epicScores - HashMap of each page that has one of the words in the query present
   * so the HashMap maps ids of pages -> their score for the query
   * @return HashMap of each page that has one of the words in the query present
   * so the HashMap maps ids of pages -> their score for the query with the iceRank 
   * of every page incorporated into each id's score
   */
  private def iceRank(epicScores: HashMap[Int, Double]) : HashMap[Int, Double] = {
    for ((id, score) <- epicScores){
      this.iceRanks.get(id) match {
        case None => 
        case Some(rank) => epicScores.update(id, score*rank)
      }
    }
    return epicScores
  }
  
  
  
  
  /**Takes in a HashMap of all the ids -> scores for the pages that have any 
   * results for the query, puts all these values into a priority queue, and then
   * prints out the top 10 results
   * 
   * @param epicScores - a HashMap of all the ids -> scores for the pages that have any 
   * results for the query
   */
  private def topTen(epicScores: HashMap[Int, Double]) = {
    val pq: PriorityQueue[(String, Double)] = new PriorityQueue()(Ordering.by(_._2))
    for ((id, score) <- epicScores){
      titles.get(id) match {
        case None => 
        case Some(title) => pq.enqueue((title, score))
      }
    }
    var counter: Int = 1
    if (pq.isEmpty){
      println("Sorry, there were no results matching your query.")
    }
    pq.reverse
    while (!pq.isEmpty && counter <= 10){
      val(page, _) = pq.dequeue
      println(counter + ": " + page)
      counter += 1
    }
  }
}



/**Query companion object that actually allows the user to run a Query
 * 
 */
object Query {
  /**Main method that creates an instance of query whenever called, and 
   * calls runQuery to actually get the user's input until the user wants to stop
   */
  def main(args: Array[String]) = {
    if (args.size == 2 || args.size == 3){
      try {
        var q :Query = null
        if (args(0).equals("--ice-rank") && args.size == 3){
          q = new Query(false, args(1), args(2))
        } else {
          q = new Query(true, args(0), args(1))
        }
        q.runQuery
      } catch {
        case e : FileNotFoundException => println("We couldn't find your file")
        case f : NumberFormatException => println("You put in a bad file NF!")
        case g : IllegalArgumentException => println("You put in a bad file IA!")

      }
    } else {
      println("Hey, try the right number of arguments (2 or 3) next time.")
    }
  }
  
}