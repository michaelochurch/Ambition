// Neural.scala

// Note: This is not intended as a general-purpose neural network library. It's
// to be used for training neural nets to understand board and card games, over
// which the input space is small. Thus, I'm favoring simplicity of code over
// performance, because the training process need not be done in "real time". 

// I apologize for the imperative nature of this code. What's being done here is
// innately imperative and I see no purpose in pretending otherwise. However, I
// will be frequently copying network states, and treating them as immutable
// outside of training.

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

object Neural {
  val DefaultRNG = new Random()

  def normal(rng:Random = DefaultRNG) = {
    // approximate and slow, but good enough for our purposes. 
    (1 to 12).map(_ => rng.nextDouble()).foldLeft(-6.0)(_ + _)
  }

  def average(xs:Iterable[Double]) = {
    val (sum, count) = 
      xs.foldLeft((0.0, 0))((accAndCount, elem) => {
        val (acc, count) = accAndCount
        (acc + elem, count + 1)})
    sum / count
  }

  // TODO (if needed) : DataSet type for multiple output nodes. 
  case class DataSet(inputs:Iterable[Array[Double]], targets:Iterable[Double])

  trait PerceptronTransform {
    val name : String
    
    def apply(x:Double):Double
    def derivative(x:Double):Double
  }

  object IdentityTransform extends PerceptronTransform {
    val name = "IdentityTransform"
    def apply(x:Double) = x
    def derivative(x:Double) = 1.0
  }

  object ArctanTransform extends PerceptronTransform {
    val name = "ArctanTransform"
    def apply(x:Double) = (2.0 / math.Pi) * math.atan(x)
    def derivative(x:Double) = (2.0 / math.Pi) / (1.0 + x * x)
  }

  // Single-threaded mutable Perceptron. 
  class Perceptron(val weights:Array[Double], val transform:PerceptronTransform) {
    def this(dim:Int, transform:PerceptronTransform) = 
      this(Array.fill(dim + 1)(normal() * math.pow(dim + 1, -0.5)),
           transform)

    def dim = weights.length - 1
    val gradient    = Array.fill(dim + 1)(0.0)  // Will be mutated. 
    var nDataPoints = 0

    var fired       = false
    var lastInput   = null : Array[Double]
    var dotProduct  = 0.0
    var output      = 0.0
    var error       = 0.0


    def copy() = new Perceptron(weights.clone, transform)

    // single-threaded / mutable.  fire must never change the array,
    // updates. When we run our network against our test (as opposed to
    // training) set, we are _not allowed_ to update the network. 
    def fire(input:Array[Double]) = {
      // TODO: Make logging behavior configurable. 
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

    def train(inError:Double) = {
      assert(fired)
      error = inError
      for (i <- 0 until gradient.length) {
        gradient(i) -= error * transform.derivative(dotProduct) * (if (i == 0) 1.0 else lastInput(i - 1))
      }
      nDataPoints += 1
      fired = false
    }

    def trainKnownTarget(input:Array[Double], target:Double) = {
      train(output - target)
    }

    def applyUpdates(learningRate:Double) = {
      if (nDataPoints > 0) {
        for (i <- 0 until weights.length) {
          weights(i) += gradient(i) * learningRate / nDataPoints
          gradient(i) = 0.0
        }
        nDataPoints = 0
      }
    }

    // full cycle fire-and-train. 
    def fullCycle(data:Iterable[Array[Double]], targets:Iterable[Double], learningRate:Double) = {
      for ((input, target) <- data.zip(targets)) {
        fire(input)
        trainKnownTarget(input, target)
      }
      applyUpdates(learningRate)
    }

    def l2Error(data:Iterable[Array[Double]], targets:Iterable[Double]) = {
      average(data.zip(targets).map { case (input, target) => math.pow((target - apply(input)), 2.0) })
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
      for (i <- 1 to 4000) {
        p.fullCycle(inputs, targets, 0.01) 
        printf("Step #%d\n", i)
        printf("Y = %.4f + %.4f * X1 + %.4f * X2 + %.4f * X3\n", p.weights(0), p.weights(1), p.weights(2), p.weights(3))
        printf("L^2 total error : %.6f\n", p.l2Error(inputs, targets))
      }
      p
    }
  }

  class LayeredNetwork(val layers:Array[Array[Perceptron]]) {
    var frozen = false  // If true, operations that modify the weights are not allowed. 
    def freeze() = {
      frozen = true
    }

    def checkUnfrozen() = {
      if (frozen) throw new Exception("Network is frozen. No side effects allowed.")
    }
    
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

    def backPropagate(layerIndex:Int, error:Array[Double]) = {
      val backErrors = Array.fill(layers(layerIndex)(0).dim)(0.0)
      for ((perceptron, i) <- layers(layerIndex).zipWithIndex) {
        perceptron.train(error(i))
        for (j <- 1 to perceptron.dim) {
          backErrors(j - 1) += error(i) * perceptron.weights(j)
        }
      }
      backErrors
    }
    
    def applyUpdates(learningRate:Double) = {
      checkUnfrozen()
      for (layer <- layers) {
        for (perceptron <- layer) {
          perceptron.applyUpdates(learningRate)
        }
      }
    }

    // Using this method will seriously impede performance (which we don't care
    // about for "batch jobs" to develop position-evaulation heuristics) but it
    // allows us to "go back in time" when training the network.
    def copy() = {
      val perceptrons = 
        layers.map { layer => 
          Array.tabulate(layer.length)(i => layer(i).copy())
      }
      new LayeredNetwork(perceptrons)
    }
    
    var aFrozenCopy = None : Option[LayeredNetwork]
    def frozenCopy() = {
      aFrozenCopy match {
        case Some(c) => c
        case None => {
          val theCopy = this.copy()
          theCopy.freeze()
          aFrozenCopy = Some(theCopy)
          theCopy
        }
      }
    }

    def fullCycle(inputs:Iterable[Array[Double]], targets:Iterable[Array[Double]], learningRate:Double):Unit = {
      var totalError = 0.0
      var dataPoints = 0
      for ((input, target) <- inputs.zip(targets)) {
        var currentSignals = input
        for (layerIndex <- 0 until layers.length) {
          currentSignals = feedForward(layerIndex, currentSignals)
        }
        val output = currentSignals
        val error = output.zip(target).map { case (o, t) => o - t }
        totalError += error.map(x => x * x).sum
        dataPoints += 1

        var currentBackSignal = error
        for (layerIndex <- layers.length - 1 to 0 by -1) {
          currentBackSignal = backPropagate(layerIndex, currentBackSignal)
        }
      }
      applyUpdates(learningRate)
    }

    // Fsck type erasure. 
    def fullCycle1(inputs:Iterable[Array[Double]], targets:Iterable[Double], learningRate:Double):Unit = {
      fullCycle(inputs, targets.map(Array(_)), learningRate)
    }

    // The .apply method must *never* side-effect the weights. (It's used on
    // validation-set data, which cannot be used in training.)
    def apply(input:Array[Double]):Double = {
      var currentSignal = input
      for (layerIndex <- 0 until layers.length) {
        currentSignal = feedForward(layerIndex, currentSignal)
      }
      currentSignal(0)
    }
   
    // for debugging... 
    def digest() = {
      layers.toList.map(_.toList).hashCode
    }
 
    def l2Error(ds:DataSet) = {
      average(ds.inputs.zip(ds.targets).map { case (input, target) => math.pow((apply(input) - target), 2.0) })
    }
  }

  object LayeredNetwork {
    def apply(spec:IndexedSeq[Int], transforms:IndexedSeq[PerceptronTransform]) = {
      val layers = new ArrayBuffer[Array[Perceptron]]()
      for (i <- 1 until spec.length) {
        layers.append(Array.fill(spec(i))(new Perceptron(spec(i - 1), transforms(i - 1))))
      }
      new LayeredNetwork(layers.toArray)
    }
  }

  def main(args:Array[String]) = {
    LayeredNetwork.demo2(250)
  }

  // Solver, which trains a neural network using a learning rate that increases
  // on successful steps and declines on failed ones.
  class Solver(dimensions:IndexedSeq[Int], initLearningRate:Double, trainingData:DataSet, validationData:DataSet) {
    val lrAdjustOnSuccess = 1.01
    val lrAdjustOnFailure = 0.5

    var learningRate = initLearningRate
    val transforms = Array.tabulate(dimensions.length - 1)(i => if (i == dimensions.length - 2) IdentityTransform else ArctanTransform)
    var currentNetwork = LayeredNetwork(dimensions, transforms)
    var currentNetworkTrainingError = currentNetwork.l2Error(trainingData)
    var bestNetwork = currentNetwork  
    // Best network according to validation data. The only use of this data we
    // are allowed is to select the best network at the end of the training
    // process. We can't inject validation data into training itself.

    var bestNetworkValidationError = Double.PositiveInfinity
    var bestNetworkStepNumber = -1
    var nSteps = 0
    var nFailedSteps = 0
    
    def step() = {
      // TODO: one of these copies (current -> new, new -> current) is probably unnecessary. 
      val newNetwork = currentNetwork.copy()
      newNetwork.fullCycle1(trainingData.inputs, trainingData.targets, learningRate)
      val newTrainingError = newNetwork.l2Error(trainingData)
      // fullCycle1 side-effects newNetwork (1 step of training)
      if (newTrainingError <= currentNetworkTrainingError) {
        nSteps += 1
        learningRate *= lrAdjustOnSuccess
        currentNetwork = newNetwork.frozenCopy()
        currentNetworkTrainingError = newTrainingError
        val validationError = newNetwork.l2Error(validationData)
        if (validationError < bestNetworkValidationError) {
          bestNetwork = newNetwork.frozenCopy()
          bestNetworkValidationError = validationError
          bestNetworkStepNumber = nSteps
        }
        printf("Step #%d. TSet Error: %.6f VSet Error: %.6f LR: %.6f\n", nSteps, newTrainingError, validationError, learningRate) 
      } else {
        nSteps += 1
        printf("Failed step: (error jumped to %.6f from %.6f)\n", newTrainingError, currentNetworkTrainingError)
        nFailedSteps += 1
        learningRate *= lrAdjustOnFailure
      }
    }

    def run(n:Int) = {
      val targetnSteps = nSteps + n
      while (nSteps < targetnSteps) {
        step()
      }
    }
  }

  object Solver {
    def demo(networkDim:Array[Int] = Array(4, 20, 1), nSteps:Int = 1000) = {
      def f(xs:Array[Double]) = 0.2 - 0.3 * xs(0) * xs(1) + 0.5 * xs(2) - 0.4 * math.pow(xs(3), 3.0)
      val TrainingRange = -1.0 to 1.0 by 0.5 toArray
      val TrainingInputs = for (x <- TrainingRange; y <- TrainingRange; z <- TrainingRange; w <- TrainingRange) yield Array(x, y, z, w)
      val TrainingSet = DataSet(TrainingInputs, TrainingInputs.map(f))

      val ValidationRange = -0.75 to 0.75 by 0.5 toArray
      val ValidationInputs = for (x <- ValidationRange; y <- ValidationRange; z <- ValidationRange; w <- ValidationRange) yield Array(x, y, z, w)
      val ValidationSet = DataSet(ValidationInputs, ValidationInputs.map(f))
      
      val s = new Solver(networkDim, 0.05, TrainingSet, ValidationSet)
      s.run(nSteps)
      s
    }
  }
}
