package cosc250.assignmentOne

import scala.Option


import scala.annotation.tailrec

object AssignmentOne {

  /**
    * This is a sequence that John Conway (famous for the game of life) offered a prize about at a symposium at Bell
    * Labs a long time ago.
    *
    * a(1) = 1
    * a(2) = 1
    * a(n) = a(a(n - 1)) + a(n - a(n - 1)), for n > 2
    *
    * You can find more on the sequence here
    * https://oeis.org/A004001
    *
    * Note that this problem is "1-indexed" -- ie, we're only defining the sequence starting at 1 instead of 0
    *
    * Your function should produce the sequence from 1 to n (inclusive)
    *
    * For style marks, use some memoisation so that you don't keep recomputing the same values.
    * (But you don't need to produce a tail recursive solution)
    */
  def conwaySequence(n: Int): Seq[Int] = {

    (0 to n - 1).foldLeft(Vector[Int]()) { (v, idx) =>
      if (idx <= 1) v :+ 1 else v :+ (v(v(idx - 1) - 1) + v(idx - v(idx - 1)))
    }
  }

  /**
    * Use foldLeft to sum the number of characters in a tree of strings
    */

// start with a value of zero and then keep track of the total with sum
// at each step the length of the next string is added to the total
  def countOfTree(tree: scala.collection.immutable.TreeSet[String]): Int = {

    tree.foldLeft(0) { (sum, i) => sum + i.length() }

  }


  // this works too .. using foldleft took hours OMG  !!
  /** {
    * *
    * var sum:Int =0;
    * for ( c <- tree )
    * { for ( x <- c ) { sum = sum + 1 } }
    * sum
    * *
    *
    * } */


  /**
    * Let's implement a Scrabble scorer.
    * It should take into account the letters in the word *and* the squares the word sits on.
    *
    * I've created a sealed trait to model the different kinds of square. A "sealed trait" means that every class or
    * object that implements that trait is defined in the same program file. Knowing that there aren't any other
    * potential Squares out there (eg, being added later by other programmers) means the compiler can do cleverer
    * exhaustiveness-checking for us.
    *
    */

  sealed trait Square

  case object OrdinarySquare extends Square

  case object DoubleLetterScore extends Square

  case object TripleLetterScore extends Square

  case object DoubleWordScore extends Square

  case object TripleWordScore extends Square

  /**
    * (You may assume all letters are uppercase)
    *
    * 1: A, E, I, O, U, L, N, S, T, R.
    * 2: D, G.
    * 3: B, C, M, P.
    * 4: F, H, V, W, Y.
    * 5: K
    * 8: J, X.
    * 10: Q, Z.
    *
    * You might find using "mystring".contains(char) useful to keep it short
    */
  def letterScore(char: Char): Int = {

    // create cases for each point
    val onePoints: String = {
      "AEIOULNSTR"
    }
    val twoPoints: String = {
      "DG"
    }
    val threePoints: String = {
      "BCMP"
    }
    val fourPoints: String = {
      "FHVWY"
    }
    val fivePoints: String = {
      "K"
    }
    val eightPoints: String = {
      "JX"
    }
    val tenPoints: String = {
      "QZ"
    }


    if (onePoints.contains(char)) {
      1
    }
    else if (twoPoints.contains(char)) {
      2
    }
    else if (threePoints.contains(char)) {
      3
    }
    else if (fourPoints.contains(char)) {
      4
    }
    else if (fivePoints.contains(char)) {
      5
    }
    else if (eightPoints.contains(char)) {
      8
    }
    else if (tenPoints.contains(char)) {
      10
    }
    else {
      0
    }


  }

  /**
    * This should work out what this letter scores, given the square it's on.
    * Don't forget - DoubleWordScores etc affect the word as a whole, not individual letters
    */
  def letterAndSquareScore(char: Char, sq: Square): Int = {
    if (sq == OrdinarySquare) {
      letterScore(char)
    }
    else if (sq == DoubleLetterScore) {
      letterScore(char) * 2
    }
    else if (sq == TripleLetterScore) {
      letterScore(char) * 3
    }
    else {
      letterScore(char)
    }

  }


  /**
    * Calculate the scrabble score for a word on a set of squares.
    *
    * Hint: the zip method on Seq will zip to sequences together into a sequence of tuples. For example,
    *
    * Seq(1, 2, 3).zip(Seq("A", "B", "C")) produces Seq((1, "A"), (2, "B"), (3, "C")).
    *
    * If the sequences are of lengths, it'll just zip as much as it can. For example,
    * Seq(1, 2).zip(Seq("A", "B", "C")) produces Seq((1, "A"), (2, "B")).
    *
    * Tuples can be accessed using _1 and _2
    * val tup = (1, 2)
    * tup._1 == 1
    *
    * or using destructuring assignmnet
    * val (x, y) = tup
    *
    */
  // store mutable list of double and triple word scores
  // scan entire word looking for double and triple word scores

  // multiply all values in that list for a multiplication factor
  // scan the word multiplying each letter by it's square value and store a running total of points
  // multiply total points by multiplication factor
  def scrabbleScore(word: String, squares: Seq[Square]): Int = {

    // var to store total multiplication of double triple word scores
    var multiplicationFactor: Int = 1
    // var to store total of just letters times double letters etc for entire word
    // multiplicationFactor * wordLetterSquareSum is desired output
    var wordLetterSquareSum: Int = 0
    var result: Int = 0;

    // lets calculate wordLetterSquareSum first
    // by iterating across the string and adding the value of each char to the total
    // how the heck do I do this in Scala .
    var index: Int = 0;
    for (c <- word) {
      wordLetterSquareSum = wordLetterSquareSum + letterAndSquareScore(c, squares(index));
      index = index + 1;
    }
    // hopefully that's enough to count up the sum
    // we can do a similar thing for the mult factor
    // reset the index back to zero. sorry index you have to pay for your crimes  !!
    index = 0;


    for (c <- word) {
      if (squares(index) == DoubleWordScore) {
        multiplicationFactor = multiplicationFactor * 2
      }
      else if (squares(index) == TripleWordScore) {
        multiplicationFactor = multiplicationFactor * 3
      }
      else {
        multiplicationFactor = multiplicationFactor * 1
      }

      index = index + 1;
    }

    // combine the letter total with the double triple word scores
    result = wordLetterSquareSum * multiplicationFactor;
    // send it back out
    result;


  }

  /**
    * Let's solve the "8 queens" problem -- how to put eight queens on a chessboard without any of them attacking
    * each other.
    */

  /**
    * A position on the board. For our purposes, columns and rows are numbered from 1 to 8
    */
  case class Pos(x: Int, y: Int)

  /** Are two positions in the same row? */
  // the y value is the "row" we need to compare them
  def sameRow(p1: Pos, p2: Pos): Boolean = {
    if (p1.y == p2.y) {
      true

    } else {
      false
    }
  }

  /** Are two positions in the same column? */
  // the x value is the column we need to compare them
  def sameCol(p1: Pos, p2: Pos): Boolean = {
    if (p1.x == p2.x) {
      true
    } else {
      false
    }
  }

  /** Are two positions on the same diagonal? Remember, there are two diagonals to worry about. */
  def sameDiagonal(p1: Pos, p2: Pos): Boolean = {
    /// diagonals have are equal distance from the source in both directions
    // absolute value removes up or down .. either direction will work
    if (math.abs(p1.x - p2.x) == math.abs(p1.y - p2.y)) {
      true
    } else {
      false
    }
  }


  /**
    * Now let's define a function to test whether queens in two positions are attacking each other.
    * Don't forget a queen cannot attack itself. ie, (4,4) is not attacking (4,4)
    */
  def attackingEachOther(p1: Pos, p2: Pos): Boolean = {

    // we have to test all the cases of being on the same column , row or diagonal
    if ((p1.x == p2.x) && (p1.y == p2.y)) {
      false
    }
    else if (sameRow(p1, p2)) {
      true
    }
    else if (sameCol(p1, p2)) {
      true
    }
    else if (sameDiagonal(p1, p2)) {
      true
    }
    else {
      false
    }
  }


  /**
    * Using your attackingEachOther method, write a function that looks through a sequence of positions and finds if
    * there are any queens attacking each other
    */
  def seqContainsAttack(queens: Seq[Pos]): Boolean = {
    /// iterate through all positions and use a boolean rslt to be triggered if an attack occurs
    var reslt: Int = 0
    for (i <- queens; j <- queens)
      if (attackingEachOther(i, j)) {
        reslt = reslt + 1
      }
      else {}
    if (reslt > 0) {
      return true
    }
    else (return false)

  }

  /**
    * This method should take a sequence of rows. To solve eight queens, all of the queens must be in different columns.
    * So, rather input the full positions for each queen, we can just take (in order) the row number for each column.
    * ie, Seq(1, 8, 2, 7) would mean there's a queen at (1,1) another at (2, 7), another at (3, 2), another at (4, 7).
    *
    * Use your seqContainsAttack function to work out whether a sequence in this format contains an attack. You might
    * find the "zipWithIndex" function helpful. This starts at zero, not one, which might or might not make a difference.
    */
  def seqContainsAttackI(queens: Seq[Int]): Boolean = {

    def seqToPos(inputSeq: Seq[Int]): Seq[Pos] = {
 //// create a list of Pos
      for {

        (y, x) <- queens.zipWithIndex

      } yield Pos(x, y)
    }
 //// convert the input into a Seq of positions
    var k = seqToPos(queens)
/// pass this Seq into the seqContainsAttack function
    if (seqContainsAttack(k) == true) {
      return true
    } else {
      return false
    }

  }


  /**
    * Now we're going to use another trick to make the whole computation very small. As well as the queens all being
    * in different columns, they're also all in different rows. So every solution is going to be a permutation of
    * Seq(1, 2, 3, 4, 5, 6, 7, 8). But we're going to need to filter the permutations to only the ones that work.
    *
    * Write your function to calculate all 92 solutions to the eight queens problem
    *
    * You might find the following useful:
    * - permutations will produce an Iterator across the permutations of a sequence
    * - filterNot will filter a Seq, Iterator, etc, to only those where a particular function returns false
    * - toSeq will turn a List, Iterator, etc, into a Seq (sequence)
    */
  def eightQueens: Seq[Seq[Int]] = {

    var s = Seq(1, 2, 3, 4, 5, 6, 7, 8)
    // create a huge Seq with all possible permutations
    var sp = s.permutations.toList
    // filter all the permutations looking for an attack
    var spf = sp.filterNot(seqContainsAttackI)
    return spf

  }


  /**
    * Given two strings of equal length, calculate the number of characters that are different in each string.
    *
    * eg, "BLAB" and "BLOB" are 1 character different. "RAM" and "MAR" are 2 characters different.
    */
  def numDiffChars(a: String, b: String): Int = {

    var t: Int = 0
    /// iterate through the 2 strings comparing chars and summing the total
    for (c <- 0 to a.length - 1) if (a(c) != b(c)) {
      t = t + 1
    }
    return t

  }

  /**
    * I'll give you this method -- it takes a sequence and returns a sequence of tuples, with each pair. eg,
    * seqToPairs(Seq(1, 2, 3)) would produce Seq((1, 2), (2, 3))
    */
  def seqToPairs[T](s: Seq[T]): Seq[(T, T)] = s zip s.tail

  /**
    * Given a sequence of strings, sum the character changes that would be needed to change each string to the next
    * eg:
    *
    * SUM
    * SAM  (1 change)
    * BAT  (2 changes)
    * total: 3 changes
    */
  def sumChanges(s: Seq[String]): Int = {


    var tot: Int = 0
    var sp = seqToPairs(s)
    for (each <- sp) {
      tot = tot + numDiffChars(each._1, each._2)
    }
    return tot


  }

  /**
    * Given a sequence of strings, reorder the strings so that sumChanges will be as small as possible.
    * A sequence and its reverse will have the same length.
    */
  def smallestChanges(strings: Seq[String]): Seq[String] = {

    var scc = strings.permutations.toList


// create a starting point
    var fir = sumChanges(scc(1))
    var res: Int = 0

    var lowest: Int = sumChanges(scc(0))

    for (i <- 0 to scc.length - 1) (if (sumChanges(scc(i)) < lowest) {
      res = i;
      lowest = sumChanges(scc(i))
    } else {})

    return scc(res)
  }


  /**
    * Now lets make a little calculator
    * Again I've got you started by defining a sealed trait and a few case classes
    */
  sealed trait Expression {

    /** result should calculate the answer. You should implement it in the subclasses. */
    def result: Double

    /**
      * Implement foldLeft... If you've got it right, then count will work
      *
      * Normally, we define foldLeft on a list, but actually it can be generalised to data types such as trees.
      * Technically, it then gets called a "catamorphism", but let's just stick with foldLeft
      *
      * You might find it helpful to sketch a little expression tree, and then think about
      * - what you would do for leaf nodes
      * hint: you can work that out from the types --- you have a function that takes a value, and a node containing a value
      * - what you would do for non-leaf nodes
      * hint: just work it out for the three-node tree. And remember you are processing from left to right
      */
    def foldLeft[A](start: A)(f: (A, Expression) => A): A

    /** Traverses the nodes from left to right, counting them if they match a condition. */
    def count(f: Expression => Boolean) = foldLeft(0) {
      case (counted, expr) if f(expr) => counted + 1
      case (counted, _) => counted
    }
  }

  case class Number(i: Int) extends Expression {
    // if we are calculating a number then we just need to return the value
    override def result = i


    override def foldLeft[A](start: A)(f: (A, Expression) => A) = {
      // I attempted for foldleft to do nothing if it was looking at a leaf
      foldLeft(start) { case (n, start) => n }
    }

  }


  case class Add(left: Expression, right: Expression) extends Expression {
    override def result = {
      // calculate just needs to do the calculation for each value since the brackets in the expression will
      // take care of everything else
      calculate(left) + calculate(right)
    }

    override def foldLeft[A](start: A)(f: (A, Expression) => A) = {


      // maybe case of left being a number or right being a number could be compared ?
      // if this starts with number
      // case (left, Number(i))
       if (this.left.toString == "Number")
       {val newStart = f(start, this)
      right.foldLeft(newStart)(f)}
       else if (this.right.toString == "Number")
       {val newStart = f(start, this)
         left.foldLeft(newStart)(f)}
       else {val newStart = f(start, this)
         foldLeft(newStart)(f)}

    }
  }




  case class Multiply(left: Expression, right: Expression) extends Expression {
    override def result = {
      calculate(left) * calculate(right)
    }

    override def foldLeft[A](start: A)(f: (A, Expression) => A) = {
      val newStart = f(start, this)
      right.foldLeft(newStart)(f)
    }
  }



  case class Subtract(left: Expression, right: Expression) extends Expression {
    def result = {
      calculate(left) - calculate(right)
    }

    def foldLeft[A](start: A)(f: (A, Expression) => A) =
    {
      val newStart = f(start, this)
      right.foldLeft(newStart)(f)
    }
  }

    /** Calculate the result of an expression */
    def calculate(ex: Expression) = ex.result

    /** Now implement a function that will count how many Multiply nodes there are in the calculation */
    def countMultiplications(ex: Expression): Int =
      ex.toString().sliding("Multiply".length).count(_ == "Multiply")

  /// or will's recursive solution :
    // if (ex.getClass.getTypeName == "Number" ) {val rslt:Int = 0}
    // else if (ex.getClass.getTypeName == "Multiply") {val rslt:Int = countMultipliations(left}





    /**
      * We started with John Conway -- let's finish with his most famous creation: Conway's Game of Life.
      * https://en.wikipedia.org/wiki/Conway's_Game_of_Life
      *
      * Suppose we have a grid of squares, say 20 by 20
      * And a square can be filled in (alive) or not filled in (dead).
      *
      * And at each "time step", we generate a new grid using the following rules:
      * Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
      * Any live cell with two or three live neighbours lives on to the next generation.
      * Any live cell with more than three live neighbours dies, as if by overpopulation.
      * Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
      * (Each cell has eight neighbours)
      *
      * We're going to define the game using an immutable Map.
      * Here, I've used Scala's "type alias" syntax to say that a ConwayState is a map from a tuple of ints to a boolean
      *
      * The tuple is going to contain (x, y) coordinates, and the Boolean is going to contain the values.
      * If an element in the map is missing, assume it to be false (dead). You can use getOrElse for this. This also
      * has the advantage that we *can* ask about negative indices -- getOrElse((-1, -1), false) will be false
      *
      */
    type ConwayState = Map[(Int, Int), Boolean]

    /**
      * Blinkers have a habit of toggling -- to help you test your code, I've included their definition.
      * If you have a blinker1, and you move forward one tick in the game state, you should get blinker2.
      * See the wikipedia page for more on this.
      */
    val blinker1: ConwayState = Map(
      (2, 1) -> true, (2, 2) -> true, (2, 3) -> true
    )
    val blinker2: ConwayState = Map(
      (1, 2) -> true, (2, 2) -> true, (3, 2) -> true
    )

    /**
      * First, define a function that given a tuple and a ConwayState will count the number of live neighbours
      */
    def liveNeighbours(pos: (Int, Int), state: ConwayState): Int = {
      /// eight neighbors (x+1,y) (x-1,y) (x,y-1) (x,y+1) (x+1, y+1) ( x+1 , y-1 ) (x-1, y+1) (x-1, y-1 )

      //// val words = Map(1000 -> "one-thousand", 20 -> "twenty")

      // This returns None as the Option has no value.
      /// val result = words.get(2000)
      // println(result)

      // Use getOrElse to get a value in place of none.
      // val result2 = result.getOrElse("unknown")
      // println(result2)

      // counter to count the number of live neighbors
      var res: Int = 0;
      def isAlive(posi: (Int, Int)): Boolean = state.getOrElse(posi, default = false);

      // create set of neighbors :
      val xx: Int = pos._1
      val yy: Int = pos._2
      val neighbors = Seq((xx + 1, yy), (xx - 1, yy), (xx, yy - 1), (xx, yy + 1), (xx + 1, yy + 1), (xx + 1, yy - 1), (xx - 1, yy + 1), (xx - 1, yy - 1))
      for (t <- 0 to 7) (if (isAlive(neighbors(t)) == true) {
        res += 1
      } else {})
      return res

    }

    /**
      * Next, define a function that determines whether a position should be alive or dead
      */
    def aliveOrDead(pos: (Int, Int), state: ConwayState): Boolean = {


      var alive: Boolean = state.getOrElse(pos, default = false);

      var neighbors = liveNeighbours(pos, state)

      var result: Boolean = false

      // * Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
      if (alive == true && neighbors < 2) {
        result =
          false
      }
      //   * Any live cell with two or three live neighbours lives on to the next generation.
      else if (alive == true && neighbors == 2) {
        result =
          true
      }
      else if (alive == true && neighbors == 3) {
        result =
          true
      }
      else if (alive == true && neighbors > 3) {
        result = false
      }
      else if (alive == false && neighbors == 3) {
        result = true
      }
      else {
        result = false
      }

      return result


    }


    /**
      * Next, define a function that will compute the next state of the game of life, for a given maximum X and Y
      */
    def nextConwayState(state: ConwayState, maxSize: (Int, Int) = (20, 20)): ConwayState = {
      // var i: Int = 0
      // var j: Int = 0
      // set up some sort of state to return values
      // var returnstate:ConwayState = Map ((0, 0) -> false )


      //  for (i <- 0 to 19)
      //    for (j <- 0 to 19)
      //     returnstate = Map ((i,j) -> aliveOrDead((i,j),returnstate) )

      //  def foreach(state: aliveOrDead => Unit): Unit
      //return returnstate
      // var posi = (0,0)
      // for (i <- 0 to 19)
      //   for (j <- 0 to 19)
      //     posi = (i,j)
      //     Map ((state(posi)) -> aliveOrDead((posi),state))

      // state.foreach( case ( (x,y), z ) => ( z = aliveOrDead((x,y),state)


      //val returnstate: ConwayState  = state.transform((k,v) => (v = aliveOrDead(k, state))
      //  return returnstate


      //def adjust[A, B](m: Map[A, B], k: A)(f: B => B) = m.updated(k, f(m(k)))
      //state = adjust(state[x,y])(aliveOrDead(x)=>y)
      //state = state.map((x,y),z) => (x)

      //state.map { case (k,v) => k -> aliveOrDead(k,state)}

      val positions = for {
 /// create a sequence of pos tuples for all possibilities
        i <- 0 to 19

        j <- 0 to 19

      } yield (i, j)
      /// map the aliveordead onto all the tuples
      val values = positions.map { case (k, v) => (k, v) -> aliveOrDead((k, v), state) }
      /// convert to a map to return
      return values.toMap


    }




}
