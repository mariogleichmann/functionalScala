package abstract_types

object PhantomTypes {

	// phantom types - no instances (values) needed (just pure types for compile time checks in this case)
  sealed trait NoFuel
  sealed trait Fueled
  sealed trait NoO2
  sealed trait HasO2

  final case class Rocket[Fuel, O2]
  
  def createRocket = Rocket[NoFuel, NoO2]()

  def addFuel[O2](x : Rocket[NoFuel, O2]) = Rocket[Fueled, O2]()  // O2 is a type parameter -> lets 2nd parameter type untouched
  
  def addO2[FUEL](x : Rocket[FUEL, NoO2]) = Rocket[FUEL, HasO2]() // FUEL is a type parameter -> lets 1st parameter type untouched
  
  def launch(x : Rocket[Fueled, HasO2]) = "blastoff"
	  
	
  implicit def toPiped[V] (value:V) = new {
    def |>[R] (f : V => R) = f(value)
  }

  def test1 = createRocket |> addFuel |> addO2 |> launch
  def test2 = createRocket |> addO2 |> addFuel |> launch

  //This won't compile - there's no fuel
  //def test3 = createRocket |> addO2 |> launch
  
  // This won't compile either - there's no o2
  //def test4 = createRocket |> addFuel |> launch
  
  // This won't compile because you can't add fuel twice
  // def test5 = createRocket |> addFuel |> addO2 |> addFuel |> launch




  
}