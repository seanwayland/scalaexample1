
/**
  * Created by seanwayland on 3/19/18.
  */
class test {

  def conwaySequence(n: Int): Seq[Int] = {

    // declare an array of length n
    // supposedly an array can be returned as a seq
    val a = Array.ofDim[Int](n)

    // define the first 2 elements
    a(0) = 1
    a(1) = 1

    // iterate and populate array
    for (i <- 2 to n)

    a(i) = a(a(i - 1)) + a(i - a(i - 1))
    return a

  }

  conwaySequence(2)

}