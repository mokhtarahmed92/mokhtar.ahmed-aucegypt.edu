package monads.withmonads


import monads._
import monads.Stubs._
import StateMonad.State._

object PriceCalculatorWithMonads {

  def findBasePrice(ps: PriceState): Double = {
    val basePrice = findTheBasePrice(ps.productId)
    basePrice
  }

  def applyStateSpecificDiscount(ps: PriceState): Double = {
    val discount = findStateSpecificDiscount(ps.productId, ps.stateCode)
    ps.price - discount
  }

  def applyProductSpecificDiscount(ps: PriceState): Double = {
    val discount = findProductSpecificDiscount(ps.productId)
    ps.price - discount
  }

  def applyTax(ps: PriceState): Double = {
    val tax = calculateTax(ps.productId, ps.price)
    ps.price + tax
  }

  def modifyPriceState(f: PriceState => Double): StateMonad.State[PriceState, Unit] = modify[PriceState](s => s.copy(price = f(s)))

  def logStep(f: PriceState => String): StateMonad.State[PriceState, String] = gets(f)

  def calculatePrice(productId: String, stateCode: String): Double = {

    val initialPriceState = PriceState(productId, stateCode, 0.0)

    val priceCalculator: StateMonad.State[PriceState, List[String]] =
      for {
      _ <- modifyPriceState(findBasePrice)
      a <- logStep(s => "Base Price " + s)

      _ <- modifyPriceState(applyStateSpecificDiscount)
      b <- logStep(s => "After state discount " + s)

      _ <- modifyPriceState(applyProductSpecificDiscount)
      c <- logStep(s => "After product discount " + s)

      _ <- modifyPriceState(applyTax)
      d <- logStep(s => "After tax " + s)

    } yield a :: b :: c :: d :: Nil

    val finalPriceState: (PriceState, List[String]) = priceCalculator.apply(initialPriceState)

    println(finalPriceState._2.mkString("\n"))

    finalPriceState._1.price
  }

  def main(args: Array[String]): Unit = {

    print("calculated price = " + calculatePrice("id", "KRK"))

  }
}
