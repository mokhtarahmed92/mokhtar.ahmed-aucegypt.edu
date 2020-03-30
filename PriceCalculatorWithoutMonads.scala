package monads.withoutmonads

/*
Moand(
  State(Old , f: (oldState => new State)
)
)*/

/***
 * ===========================================================================================
 * This application needs to calculate a price for a product by following a sequence of steps:
 * ===========================================================================================
 *
 * 1- Find the base price of the product.
 * 2- Apply a state code-specific discount to the base price.
 * 3- Apply a product-specific discount to the result of the previous step.
 * 4- Apply tax to the result of the previous step to get the final price.
 *
 */

import monads._
import monads.Stubs._

object PriceCalculatorWithoutMonads  {

  def findBasePrice(productId: String, stateCode: String): PriceState = {
    val basePrice = findTheBasePrice(productId)
    PriceState(productId, stateCode, basePrice)
  }

  def applyStateSpecificDiscount(ps: PriceState): PriceState = {
    val discount = findStateSpecificDiscount(ps.productId, ps.stateCode)
    ps.copy(price = ps.price - discount) // returning a new price state
  }

  def applyProductSpecificDiscount(ps: PriceState): PriceState = {
    val discount = findProductSpecificDiscount(ps.productId)
    ps.copy(price = ps.price - discount) // returning a new price state
  }

  def applyTax(ps: PriceState): PriceState = {
    val tax = calculateTax(ps.productId, ps.price)
    ps.copy(price = ps.price + tax) // returning a new price state
  }

  def logStep(priceState: PriceState,  msg:String): PriceState ={
    println(msg +" "+ priceState)
    priceState
  }

  def calculatePrice(productId: String, stateCode: String): Double = {
    val d = logStep(  applyTax(
              logStep(  applyProductSpecificDiscount(
                logStep(  applyStateSpecificDiscount(
                  logStep(  findBasePrice(productId, stateCode),
                    "Base Price "))
                  ,"After state discount"))
                  ,"After product discount")),
              "After tax")
    d.price
  }

  def main(args: Array[String]): Unit = {

    print("calculated price = " + calculatePrice("id1","KRK"))

  }


}
