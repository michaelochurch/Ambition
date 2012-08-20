// Neural.scala

// Note: This is not intended as a general-purpose neural network library. It's
// to be used for training neural nets to understand board and card games, over
// which the input space is small. Thus, I'm favoring simplicity of code over
// performance, because the training process need not be done in "real time". 

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

object Neural {
  val DefaultRNG = new Random()

  def normal(rng:Random = DefaultRNG) = {
    // approximate and slow, but good enough for our purposes. 
    (1 to 12).map(_ => rng.nextDouble()).foldLeft(-6.0)(_ + _)
  }

  trait PerceptronTransform {
    def apply(x:Double):Double
    
    def derivative(x:Double):Double
  }

  object IdentityTransform extends PerceptronTransform {
    def apply(x:Double) = x
    def derivative(x:Double) = 1.0
  }

  object ArctanTransform extends PerceptronTransform {
    val Gamma = 2.4987  // Preserves SD of 1 on N(0, 1) input to atan(Gamma * x).
    def apply(x:Double) = math.atan(Gamma * x)
    def derivative(x:Double) = Gamma / (1.0 + math.pow((Gamma * x), 2.0))
  }

  // Single-threaded mutable Perceptron. 
  class Perceptron(val weights:Array[Double], val transform:PerceptronTransform) {
    def this(dim:Int, transform:PerceptronTransform) = 
      this(Array.fill(dim + 1)(normal() * math.pow(dim + 1, -0.5)),
           transform)

    def dim = weights.length - 1
    val updates    = Array.fill(dim + 1)(0.0)  // Will be mutated. 

    var fired      = false
    var lastInput  = null : Array[Double]
    var dotProduct = 0.0
    var output     = 0.0
    var error      = 0.0

    def clear() = {
      fired = false
    }

    def copy() = new Perceptron(weights, transform)

    // single-threaded / mutable. 
    def fire(input:Array[Double]) = {
      fired = true
      lastInput = input
      dotProduct = weights(0)
      for ((x, i) <- input.zipWithIndex) dotProduct += x * weights(i + 1)
      output = transform(dotProduct)
      output
    }

    def apply(input:Array[Double]) = {
      fire(input)
    }

    def train(inError:Double, learningRate:Double) = {
      assert(fired)
      error = inError
      for (i <- 0 until updates.length) {
        updates(i) -= error * learningRate * transform.derivative(dotProduct) * (if (i == 0) 1.0 else lastInput(i - 1))
      }
      clear()
    }

    def trainKnownTarget(input:Array[Double], target:Double, learningRate:Double) = {
      train(output - target, learningRate)
    }

    def applyUpdates() = {
      for (i <- 0 until weights.length) {
        weights(i) += updates(i)
        updates(i) = 0.0
      }
    }

    // full cycle fire-and-train. 
    def fullCycle(data:Iterable[Array[Double]], targets:Iterable[Double], learningRate:Double) = {
      for ((input, target) <- data.zip(targets)) {
        fire(input)
        trainKnownTarget(input, target, learningRate)
      }
      applyUpdates()
    }

    def l2Error(data:Iterable[Array[Double]], targets:Iterable[Double]) = {
      data.zip(targets).map { case (input, target) => math.pow((target - apply(input)), 2.0) } sum
    }
  }

  object Perceptron {
    def demo() = {
      val Bit = Array(0.0, 1.0)
      val data = for (i <- Bit;
                      j <- Bit;
                      k <- Bit) yield (Array(i, j, k), -3.0 + 13.0 * i - 7.0 * j + 33.0 * k)
      val inputs = data.map(_._1)
      val targets = data.map(_._2)
      
      val p = new Perceptron(3, IdentityTransform)
      for (i <- 1 to 400) {
        p.fullCycle(inputs, targets, 0.01) 
        printf("Step #%d\n", i)
        printf("Y = %.4f + %.4f * X1 + %.4f * X2 + %.4f * X3\n", p.weights(0), p.weights(1), p.weights(2), p.weights(3))
        printf("L^2 total error : %.6f\n", p.l2Error(inputs, targets))
      }
      p
    }
  }

  class LayeredNetwork(val layers:Array[Array[Perceptron]]) {
    def validate() = {
      for (i <- 0 to (layers.length - 2)) {
        for (p <- layers(i + 1)) assert (p.dim == layers(i).length)
      }
    }
    
    def feedForward(layerIndex:Int, input:Array[Double]) = {
      for (perceptron <- layers(layerIndex)) {
        perceptron.fire(input)
      }
      layers(layerIndex).map(p => p.output)
    }

    def backPropagate(layerIndex:Int, error:Array[Double], learningRate:Double) = {
      val backErrors = Array.fill(layers(layerIndex)(0).dim)(0.0)
      for ((perceptron, i) <- layers(layerIndex).zipWithIndex) {
        perceptron.train(error(i), learningRate)
        for (j <- 1 to perceptron.dim) {
          backErrors(j - 1) += error(i) * perceptron.weights(j)
        }
      }
      backErrors
    }
    
    def applyUpdates() = {
      for (layer <- layers) {
        for (perceptron <- layer) {
          perceptron.applyUpdates()
        }
      }
    }
   
    // TODO: Write a .copy function so we can "go back in time" when training the network.  
 
    // Returns L^2 error. 
    def fullCycle(inputs:Iterable[Array[Double]], targets:Iterable[Array[Double]], learningRate:Double):Double = {
      var totalError = 0.0
      for ((input, target) <- inputs.zip(targets)) {
        var currentSignals = input
        for (layerIndex <- 0 until layers.length) {
          currentSignals = feedForward(layerIndex, currentSignals)
        }
        val output = currentSignals
        val error = output.zip(target).map { case (o, t) => o - t }
        totalError += error.map(x => x * x).sum
        
        var currentBackSignal = error
        for (layerIndex <- layers.length - 1 to 0 by -1) {
          currentBackSignal = backPropagate(layerIndex, currentBackSignal, learningRate)
        }
        
        applyUpdates()
      }
      println("Total L^2 error: " + totalError)
      totalError
    }

    // Fsck type erasure. 
    def fullCycle1(inputs:Iterable[Array[Double]], targets:Iterable[Double], learningRate:Double):Double = {
      fullCycle(inputs, targets.map(Array(_)), learningRate)
    }

    def apply(input:Array[Double]):Double = {
      var currentSignal = input
      for (layerIndex <- 0 until layers.length) {
        currentSignal = feedForward(layerIndex, currentSignal)
      }
      currentSignal(0)
    }
  }

  object LayeredNetwork {
    def apply(spec:Array[Int], transforms:Array[PerceptronTransform]) = {
      val layers = new ArrayBuffer[Array[Perceptron]]()
      for (i <- 1 until spec.length) {
        layers.append(Array.fill(spec(i))(new Perceptron(spec(i - 1), transforms(i - 1))))
      }
      new LayeredNetwork(layers.toArray)
    }

    def demo() = {
      val XorInputs = Array(Array(-1.0, -1.0), Array(-1.0, 1.0), Array(1.0, -1.0), Array(1.0, 1.0))
      val XorTargets = Array(-1.0, 1.0, 1.0, -1.0)
      val network = apply(Array(2, 3, 1), Array(ArctanTransform, IdentityTransform))
      for (i <- 1 to 200) 
        network.fullCycle1(XorInputs, XorTargets, 0.1)
      network
    }
  }
}
