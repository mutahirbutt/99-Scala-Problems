/**Representing JSON
{ "firstName": "John",
  "lastName" : "Smith",
  "address" : {
    "streetAddress": "21 2nd Street",
    "state" : "NY",
    "postalCode" : 10021
    },
    "phoneNumbers": [
    {"type" : "home", "number" : "212-344"},
    {"type" : "mobile", "number" : "646-566"}
    ]
}
*/

abstract class JSON 
object JSON:
  case class Seq(elems: List[JSON]) extends JSON 
  case class Obj(bindings: Map[String, JSON]) extends JSON 
  case class Num(num: Double) extends JSON 
  case class Str(str: String) extends JSON 
  case class Bool(b: Boolean) extends JSON 
  case object Null extends JSON 

val jsData = JSON.Obj(Map(
    "firstName" -> JSON.Str("John"),
    "lastName" -> JSON.Str("Smith"),
    "address" -> JSON.Obj(Map(
        "streetAddress" -> JSON.Str("21 2nd Street"),
        "state" -> JSON.Str("NY"),
        "postalCode" -> JSON.Num(10021)
        )),
    "phoneNumbers" -> JSON.Seq(List(
        JSON.Obj(Map(
            "type" -> JSON.Str("home"),
            "number" -> JSON.Str("212-344")
            )),
        JSON.Obj(Map(
            "type" -> JSON.Str("mobile"),
            "number" -> JSON.Str("646-566")
            )) )) ))

def show(json: JSON): String = json match
  case JSON.Null => "null"
  case JSON.Bool(b) => b.toString
  case JSON.Str(str) => s"$str"
  case JSON.Num(num) => s"$num"
  case JSON.Seq(elems) => 
    elems.map(show).mkString("[", ",","]")
  case JSON.Obj(bindings) =>
    val assoc = bindings.map(
        (key,value) => s"${key} : ${show(value)}"
    )
    assoc.mkString("{",",\n","}")

def inQuotes(str: String): String = ???