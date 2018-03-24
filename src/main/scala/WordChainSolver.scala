/**
  *
  **/
package uk.pandagrove.wcs
import scala.io.Source
import scala.collection.immutable._  

class WordChainSolver {
  val words : Set[String] =  
    Source.fromInputStream(getClass.getResourceAsStream("/dictionary.txt")).getLines.mkString.split(",").foldLeft(new HashSet[String]())((set , word : String) => set + word)

/**
  // Convert a word in to a list of words with each character in turn being replaced by "_"
*/
  def makeWordChain (word: String) : List[String] = {
    (0 to word.length-1).toList.map((index => {
      word.toLowerCase().updated(index,'_')
    }))
  }

/**
  Using makeWordChain() on each word in the dictionary to generate a large hashmap with all the words which only differ in
  one position.
*/
  val wordChains: HashMap[String,List[String]] =
    words.foldLeft(new HashMap[String,List[String]]())((map :HashMap[String,List[String]], word: String) => {
      makeWordChain(word)
        .foldLeft(map)((innermap : HashMap[String,List[String]], newWord: String) => {
          val wordList : List[String] = innermap.getOrElse(newWord,List[String]()) :+ word
          innermap + ((newWord, wordList))
      })
    })
  
  /**
    From the current word, get the list of next possible words, and insert into the queue if we haven't already encountered it.
  */
  def getNextWords(currentWord: String, currentQueueAndActiveRoutes: (List[String], HashMap[String, String])) : (List[String],HashMap[String, String]) =
    makeWordChain(currentWord) // Convert the current word in to a list of words with each character in turn being replaced by "_"
      .foldLeft( currentQueueAndActiveRoutes )( (currentQueueAndActiveRoutes , nextWord)=> {
        wordChains.getOrElse(nextWord, List[String]()) // Get the list of possible next words
          .foldLeft(currentQueueAndActiveRoutes)((currentQueueAndActiveRoutes, wordToCheck) => {
            val queue = currentQueueAndActiveRoutes._1 // Extract the current queue
            val activeRoutes = currentQueueAndActiveRoutes._2 // Extract the activeRoutes
            if (activeRoutes.contains(wordToCheck)) { // Have we already incountered this word if so ignore as the eailer version will result in the link being shorter
              currentQueueAndActiveRoutes
            } else {
              (queue :+ wordToCheck , activeRoutes + ((wordToCheck, currentWord))) // Add this word to the queue and put a route from this word back to the current word we're chceking.
            }             
          })
    })

  /**
    If the Queue is empty, then a connection was not found.
    If the head of the queue is the 'to' word then we have completed.
    Otherwise get the list of next possible words and add to the queue.

  */
  def processQueue(queue :List[String], to: String ,activeRoutes: HashMap[String, String]) :HashMap[String, String]  =
    queue match { 
    case Nil => activeRoutes
    case (x::xs) => {
      if (x == to ) {
        activeRoutes
      } else {
        val currentQueueAndActiveRoutes = ( xs, activeRoutes)
        val nextQueueAndActiveRoutes = getNextWords(x , currentQueueAndActiveRoutes)
        if (nextQueueAndActiveRoutes._1.length > 0) {
          processQueue(nextQueueAndActiveRoutes._1,to, nextQueueAndActiveRoutes._2)         
        } else {
          nextQueueAndActiveRoutes._2
        }
      }
    }
   }

  /**
    Working backwards from the 'to' word to the 'from' word build up the list of words.
  */
  def getWords(answer: HashMap[String, String], from: String, currentWord: String) : Vector[String]= {
    if (currentWord == from) {
      Vector[String](from)
    } else {
      getWords(answer, from , answer.getOrElse(currentWord,"")) :+ currentWord
    }
  }

  /**
    Checks to see if we can get 'from' one word 'to' another changing 1 character at a time using only words known.
  */
  def chain (from : String, to : String) : Option[Vector[String]] =
    if (from.length != to.length) {
      None
    } else {
      val answer : HashMap[String, String] = processQueue(List[String](from) , to ,new HashMap[String, String]())
      if (answer.getOrElse(to,"") == "") {
        None
      } else {
        Some(getWords(answer,from,to))
      }
    }
}

object Test extends App {
  val wcs = new WordChainSolver()

  println(wcs.chain("warm", "cold"))
  println(wcs.chain("lead", "gold"))
  println(wcs.chain("show", "boat"))
  println(wcs.chain("bare", "zyme"))
  println(wcs.chain("warm", "andi"))
}