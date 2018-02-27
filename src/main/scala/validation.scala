package object validation {
  sealed trait AddressError
  case class MissingKey(key: String) extends AddressError

  case class Form(content: Map[String, String]) {
    def get(key: String): Either[AddressError, String] =
      content.get(key).toRight(MissingKey(key))
  }

  case class Number(value: String) extends AnyVal
  case class Street(value: String) extends AnyVal
  case class City(value: String) extends AnyVal
  case class Postcode(value: String) extends AnyVal

  case class Address(number: Number, street: Street, city: City, postcode: Postcode)

  def validateNumber(form: Form): Either[AddressError, Number] = form.get("number").map(Number(_))
  def validateStreet(form: Form): Either[AddressError, Street] = form.get("street").map(Street(_))
  def validateCity(form: Form): Either[AddressError, City] = form.get("city").map(City(_))
  def validatePostCode(form: Form): Either[AddressError, Postcode] = form.get("postcode").map(Postcode(_))
}
