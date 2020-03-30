package monads

case class PriceState(productId: String, stateCode: String,price: Double)

object Stubs {

  def findTheBasePrice(productId: String) = 10.0

  def findStateSpecificDiscount(productId: String, stateCode: String) = 0.5

  def findProductSpecificDiscount(productId: String) = 0.5

  def calculateTax(productId: String, price: Double) = 5.0

}
