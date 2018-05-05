package cosc250.boids
import cosc250.boids.Simulation.current

import math.{pow, sqrt}


/**
  * A boid (bird-oid). It has a position and a velocity.
  *
  *
  * https://processing.org/examples/flocking.html
  */
case class Boid(
  position:Vec2, velocity:Vec2
) {

  /**
    * Calculates an acceleration vector that will cause it to maintain a minimum
    * separation from its closest neighbours
    * This steer is limited to maxForce
    */


  // function which returns true if to positions are within 50 pixels
  def isClose(me:Boid,you:Boid):Boolean = {

    var a:Boolean = (sqrt(pow(me.position.x - you.position.x, 2) + pow(me.position.y - you.position.y, 2)).abs < 50 )
    var b : Boolean = (sqrt(pow(me.position.x - you.position.x, 2) + pow(me.position.y - you.position.y, 2)).abs > 0 )

    var c:Boolean = a && b
    return c


  }
 // function which returns true if positions are within 25 pixels
  def isrealClose(me:Boid,you:Boid):Boolean = {

    var a:Boolean = (sqrt(pow(me.position.x - you.position.x, 2) + pow(me.position.y - you.position.y, 2)).abs < 25 )

    var b : Boolean = (sqrt(pow(me.position.x - you.position.x, 2) + pow(me.position.y - you.position.y, 2)).abs > 0 )

    //var d : Boolean = (me.position.x > 0) && (me.position.y > 0)
    var c:Boolean = a && b
    return c


  }

  def separate(others:Seq[Boid]):Vec2 = {

    /// find  all boids within 25 pixels. for each boid within 25 pixels add a force in opposite direction
    /// a force in the opposite direction just has the signs changed once the distance is calculated
    /// for each boid within 50 pixels add an acceleration vector in the opposite dir
    // sum of all these ( -(x1 - x), -(y1 - y ))
    // lets separate the x value and y value into separate Double
    // since all the maths of the additions only happens on either the x or y values
    // Separation
    // Method checks for nearby boids and steers away
    // calculate vector pointing away from neighbor .

    var closeOthers:Seq[Boid] = others.filter( isrealClose(this,_))
// create Seq of vectors of vectors pointing away
    var out1:Seq[Vec2] = for (a <- closeOthers)  yield (this.position - a.position)
// normalise each vector
   var out2:Seq[Vec2] = for (each <- out1)  yield (each.normalised)
    var dist:Seq[Double] = for (a <- closeOthers)
      yield (sqrt(pow(this.position.x - a.position.x, 2) + pow(this.position.y - a.position.y, 2)).abs)
    var out3:Seq[Vec2] = for ( a <- dist; b <- out2) yield (b/a)

    val totalForceX:Double = out3.foldLeft(0.0){ (totalForceX, i) => totalForceX + i.x }
    // sum all the y velocities
    val totalForceY:Double = out3.foldLeft(0.0){ (totalForceY, i) => totalForceY + i.y }
    // create a vector from the sum of x and y
    var totForceVec:Vec2 = new Vec2(totalForceX,totalForceY)
    // divide force by total number of near neighbors
    totForceVec/(closeOthers.length)

    //totForceVec.normalised

    return totForceVec.limit(0.03)


}

/**
* Calculates an acceleration vector that will cause it align its direction and
* velocity with other birds within Boid.neighbourDist
* This alignment force is limited to maxForce
*/


def align(others:Seq[Boid]):Vec2 = {


/// find all close boids
var closeOthers:Seq[Boid] = others.filter( isClose(this,_))
/// find the average of their velocities
// sum all the x velocities
val totalVelocityX:Double = closeOthers.foldLeft(0.0){ (totalVelocityX, i) => totalVelocityX + i.position.x }
// sum all the y velocities
val totalVelocityY:Double = closeOthers.foldLeft(0.0){ (totalVelocityY, i) => totalVelocityY + i.position.y }
// create a vector from the sum of x and y
val totVelocityVec:Vec2 = new Vec2(totalVelocityX,totalVelocityY)
// find the average velocity by dividing it by the number in the flock
val averageVelocity:Vec2 = totVelocityVec/others.length
  //averageVelocity.normalised
return averageVelocity.limit(0.03)


}

/**
* Calculates an acceleration that will steer this boid towards the target.
* The steer is limited to maxForce
*/

def seek(targetPos:Vec2):Vec2 = {

val seekVel:Vec2 = targetPos - this.position

return seekVel.limit(0.03)

}

/**
* Calculates an acceleration that will keep it near its neighbours and maintain
* the flock cohesion
*/


def cohesion(others:Seq[Boid]):Vec2 = {


// get Seq of close Boids
var closeOthers:Seq[Boid] = others.filter( isClose(this,_))
// find average position of boids related to zero
var sumPositionX:Double = 0.0
for ( each <- closeOthers)
// add the x position of each boid to the totalx
{ sumPositionX = sumPositionX  + (each.position.x - 0.0) }
var sumPositionY:Double = 0.0
// same for y values
for ( each <- others)
{ sumPositionY = sumPositionY + (each.position.y - 0.0) }
// create a Vec2 from the sum
val totalPosVec:Vec2 = new Vec2(sumPositionX,sumPositionY)
val averagePosition:Vec2 = totalPosVec/others.length
return seek(averagePosition)


}

/**
* Calculates a flocking acceleration that is a composite of its separation,
* align, and cohesion acceleration vectors.
*/
def flock(others:Seq[Boid]):Vec2 = {

  // add the 3 forces .. the weights are arbitrary
  align(others)*1.2+ cohesion(others)*1.8 + separate(others)*1.5


}


/**
* Produces a new Boid by adding the boid's velocity to its position, and adding
* the acceleration vector to the boid's velocity. Note that there is no division
* by timestep -- it's just p = p + v, and v = v + a
*
* Also note that we don't apply the limiting on maxForce in this function -- this is
* so that the startle effect can dramatically perturb the birds in a way they would
* not normally be perturbed in flight. Instead, limit maxForce in the flock function
* (or the functions it calls)
*
* We do, however, limit a boid's velocity to maxSpeed in this function. But we do it
* *before* we add the influence of the wind to the boid's velocity -- it's possible
* to fly faster downwind than upwind.
*/



def update(acceleration:Vec2, wind:Vec2):Boid = {
// add the wind to the position .. add the acceleration ( forces ) to the velocity vector
return new Boid(this.position + wind,(this.velocity + acceleration).limit(0.9))


}

def wrapX(x:Double):Double = {
if (x > Boid.maxX) x - Boid.maxX else if (x < 0) x + Boid.maxX else x
}

def wrapY(y:Double):Double = {
if (y > Boid.maxY) y - Boid.maxY else if (y < 0) y + Boid.maxY else y
}
}

object Boid {
/** How far apart the boids want to be */
val desiredSeparation = 25

/** Maximum flying velocity of a boid */
val maxSpeed = 0.5

/** maximum accelaration of a boid */
val maxForce = 0.03

/** Other boids within this range are considered neighbours */
val neighBourDist = 0

/** Wrap width of the simulation. ie, for any Boid, 0 <= x < 640 */
def maxX:Int = Simulation.width

/** Wrap height of the simulation. ie, for any Boid, 0 <= y < 480 */
def maxY:Int = Simulation.height


}


