package cosc250.boids

import scala.collection.mutable


object Simulation {

  /** Wrap width of the simulation. ie, for any Boid, 0 <= x < 640 */
  val width = 640

  /** Wrap height of the simulation. ie, for any Boid, 0 <= y < 480 */
  val height = 480

  /** How many frames of the simulation to hold */
  val frameMemory = 60

  /** How manby boids to start with in the simulation */
  val numBoids = 100

  /** When the wind is blowing, how strongly it blows */
  val windStrength = 1

  /** When the boids are startled, the strength of the vector that is applied to each of them */
  val startleStrength:Double = Boid.maxSpeed

  def startleFunction() = {

    /*
    The Startle boids button should cause the simulation, at the next time step,
    to perturb each boid by a velocity of startleStrength in a random direction
    (a different random direction for each boid)
*/
    var zilch:Vec2 = new Vec2(0,0)
    var bds = current
    var outbds = for (e <-bds) yield ( e.update(zilch,Vec2.randomDir(6)) )
    queue.enqueue(outbds)

  }

  var oneTimeFunction:Option[Boid => Vec2] = None


  /**
    * Resets the events that should occur one time only
    */

  def resetOneTimeEvents():Unit = {

    insertBoid = None
    oneTimeFunction = None
  }

  /** A mutable queue containing the last `frameMemory frames` */
  val queue:mutable.Queue[Seq[Boid]] = mutable.Queue.empty[Seq[Boid]]

  /** The wind -- an optional acceleration vector */
  var wind:Option[Vec2] = None

  /**
    * Sets a wind blowing at windStrength, at this angle.
    * Note that a northerly wind blows **from** the north, so we multiply the vector by -1.
    */
  def setWindDirection(theta:Double):Unit = {
    wind = Some(Vec2.fromRTheta(windStrength, theta))
  }

  /** A container that can hold a boid to add on the next frame */
  var insertBoid:Option[Boid] = None

  /**
    * A function that will run for the next frame only over each boid in the system,
    * producing an acceleration vector to add to a Boid
    */


  /** The current frame */
  ///
  def current:Seq[Boid] = {

      queue.last ++ insertBoid
  }

  /** Generates boids in the centre of the simulation, moving at v=1 in a random direction */
  def explosionOfBoids(i:Int):Seq[Boid] = {

      var a:Seq[Boid] =
    {
      for { n <- 0 to i}
        yield (new Boid(Vec2(height / 2, width / 2), Vec2.randomDir(1)))
    }

      return a

  }

  def sumBoidsDie() = {

    val r = new scala.util.Random
    val r1 = 200 + r.nextInt( 100);
    var theChosenOnes: Seq[Boid] =
      current.filter(_.position.x < r1)
    pushState(theChosenOnes)

  }
  /** Pushes a state into the queue */

  def pushState(boids:Seq[Boid]):Seq[Boid] = {

     queue.enqueue(boids)

    // Drops a frame from the queue if we've reached the maximum number of frames to remember
    if (queue.lengthCompare(frameMemory) > 0) queue.dequeue()
    boids
  }

  /** Called by a click to the canvas, to say that in the next frame, a boid should be inserted */
  def pushBoid(b:Boid):Unit = {
    insertBoid = Some(b)
  }

  /** Called by the Action Replay button to jump back in the memory buffer */
  def resetQueue() = {

    val beginning = queue.front
    queue.enqueue(beginning)

  }

  /** Generate the next frame in the simulation */
  def addup(a:Vec2,b:Vec2):Vec2 = {
    a + b
  }

  def swrapX(x:Double):Double = {
    if (x > width) x - width else if (x < 0) x + width else x
  }

  def swrapY(y:Double):Double = {
    if (y > height) y - height else if (y < 0) y + height else y
  }

  def update():Seq[Boid] = {
    // get current frame
    var out = current
    // get current wind
    var windy:Vec2 = wind.getOrElse(new Vec2(0,0))
    windy = windy*(-1)
    // get the wrapped positions into a Seq

    var outpos = for (a_ <- out) yield (new Vec2(swrapX(a_.position.x),swrapY(a_.position.y)))
    // get the current velocities
    var outvel = for (b_ <- out) yield (b_.velocity)
    // create tuples to generate new boids
    var outpv = outpos zip outvel
    // move the boids by adding the velocity to the position
    var outpv2 = for (a <- outpv) yield (a._1 + a._2, a._2)
    // generate new boids
    var outlist = for (c_ <- outpv2) yield ( new Boid(c_._1,c_._2))
    // generate newboids with wind added
    var outwind = for (e <-outlist) yield ( e.update(e.flock(out),windy))
    resetOneTimeEvents()
    // store the current frame
    pushState(outwind)
    // send frame to be painted
    return outwind


  }

}
