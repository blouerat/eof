class: center, middle, hidden-slide-number
count: false

# Parser Combinators

### A _type-driven_ approach to input processiEOF

---
class: middle

```tut:silent
import validation._

def validateAddress(form: Form): Either[AddressError, Address] =
  for {
    number   <- validateNumber(form)
    street   <- validateStreet(form)
    city     <- validateCity(form)
    postCode <- validatePostCode(form)
  } yield Address(number, street, city, postCode)
```

---
class: middle, poem

```
Roses are Red
Violets are Blue

Unexpected '{'
on line 32.
```

---
class: black, hidden-slide-number
background-image: url(img/glitch.jpg)

---
class: background-cover, hidden-slide-number
background-image: url(img/rubens.jpg)

---
class: black, background-contain, hidden-slide-number
background-image: url(img/sisyphe.jpg)

---
class: black, center, middle, hidden-slide-number

# EOF
---
class: background-cover, hidden-slide-number
background-image: url(img/job.jpg)

---
class: background-cover, hidden-slide-number
background-image: url(img/yak.jpg)

---
class: background-contain, hidden-slide-number
background-image: url(img/etranger.jpg)

---

```tut:silent
sealed trait Parser[A]
```

--

```tut:silent
case class Exactly(char: Char) extends Parser[Char]
```

--

```tut:silent
val a: Parser[Char] = Exactly('a')
val b: Parser[Char] = Exactly('b')
```

--

```tut:silent
def run[A](parser: Parser[A])(input: String): Option[A] =
  parser match {
    case Exactly(char) => input.headOption.filter(_ == char)
  }
```

--

```tut
run(a)("")
run(a)("z")
run(a)("a")
```

--

#👍

---

```tut:silent
sealed trait Error
case object EOF extends Error
case class Unexpected(char: Char) extends Error
```

--

```tut:silent
def run[A](parser: Parser[A])(input: String): Either[Error, A] =
  parser match {
    case Exactly(char) =>
      input.headOption.toRight(EOF).flatMap { head =>
        if (head == char)
          Right(char)
        else
          Left(Unexpected(head))
      }
  }
```

--

```tut
run(a)("")
run(a)("z")
run(a)("a")
```

--

#👍

---

```tut:silent
case class Or[A](parser1: Parser[A], parser2: Parser[A]) extends Parser[A]
```

--

```tut:silent
val aOrB: Parser[Char] = Or(a, b)
```

--

```tut:silent
def run[A](parser: Parser[A])(input: String): Either[Error, A] =
  parser match {
    case Exactly(char) =>
      input.headOption.toRight(EOF).flatMap { head =>
        if (head == char)
          Right(char)
        else
          Left(Unexpected(head))
      }
    case Or(parser1, parser2) =>
      run(parser1)(input) match {
        case Left(error)   => run(parser2)(input)
        case Right(result) => Right(result)
      }
  }
```

--

```tut
run(aOrB)("z")
run(aOrB)("a")
run(aOrB)("b")
```

---

```tut:silent
case class NEL[A](head: A, tail: List[A]) {
  def map[B](f: A => B): NEL[B] = NEL(f(head), tail.map(f))
}
```

--

```tut:silent
def oneOf[A](parsers: NEL[Parser[A]]): Parser[A] =
  parsers.tail.foldLeft(parsers.head)(Or(_, _))
```

--

```tut:silent
val allDigits: NEL[Char] = NEL('0', ('1' to '9').toList)

val digit: Parser[Char] = oneOf(allDigits.map(Exactly(_)))
```

--

```tut
run(digit)("")
run(digit)("z")
run(digit)("0")
run(digit)("1")
run(digit)("2")
run(digit)("9")
```

---

```tut
run(a)("abcd")
```

--

How about `bcd`?

--

```tut:silent
def run[A](parser: Parser[A])(input: String): Either[Error, (A, String)] =
  parser match {
    case Exactly(char) =>
      input.headOption.toRight(EOF).flatMap { head =>
        if (head == char)
          Right((char, input.tail)) // is safe, trust me
        else
          Left(Unexpected(head))
      }
    case Or(parser1, parser2) =>
      run(parser1)(input) match {
        case Left(error)   => run(parser2)(input)
        case Right(result) => Right(result)
      }
  }
```

--

```tut
run(a)("")
run(a)("z")
run(a)("abcd")
```

---

```tut:silent
case class And[A, B](
  parserA: Parser[A],
  parserB: Parser[B]
) extends Parser[(A, B)]
```

--

```tut:silent
val aAndB: Parser[(Char, Char)] = And(a, b)
```

--

```tut:silent
def run[A](parser: Parser[A])(input: String): Either[Error, (A, String)] =
  parser match {
    case Exactly(char) =>
      input.headOption.toRight(EOF).flatMap { head =>
        if (head == char)
          Right((char, input.tail))
        else
          Left(Unexpected(head))
      }
    case Or(parser1, parser2) =>
      run(parser1)(input) match {
        case Left(error)   => run(parser2)(input)
        case Right(result) => Right(result)
      }
    case And(parserA, parserB) =>
      for {
        resultA <- run(parserA)(input)
        resultB <- run(parserB)(resultA._2)
      } yield ((resultA._1, resultB._1), resultB._2)
  }
```

---

```tut
run(aAndB)("")
run(aAndB)("z")
run(aAndB)("a")
```
--

```tut
run(aAndB)("abcd")
```

---
class: middle

```tut:silent
import scalaz._
import Scalaz._
```

```tut:silent
def run[F[_], A](
  parser: Parser[A]
)(
  MS: MonadState[F, String],
  ME: MonadError[F, Error]
): F[A] = ???
```

---

```tut:silent
trait MonadStateError[F[_], S, E] extends MonadState[F, S] with MonadError[F, E]

object MonadStateError {
  def apply[F[_], S, E](
    MS: MonadState[F, S],
    ME: MonadError[F, E]
  ): MonadStateError[F, S, E] =
    new MonadStateError[F, S, E] {
      def point[A](a: => A): F[A] = MS.point(a)
      def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = MS.bind(fa)(f)
      def handleError[A](fa: F[A])(f: E => F[A]): F[A] = ME.handleError(fa)(f)
      def raiseError[A](e: E): F[A] = ME.raiseError(e)
      def get: F[S] = MS.get
      def init: F[S] = MS.init
      def put(s: S): F[Unit] = MS.put(s)
    }
}
```
---

```tut:silent
def next[F[_]](
  implicit M: MonadStateError[F, String, Error]
): F[Char] =
  M.get.flatMap {
    case ""     => M.raiseError(EOF)
    case input  => M.put(input.tail) >| input.head
  }
```
--

```tut:silent
def run[F[_], A](
  parser: Parser[A]
)(
  implicit M: MonadStateError[F, String, Error]
): F[A] =
  parser match {
    case Exactly(char) =>
      for {
        head <- next
        _    <- whenM(head != char)(M.raiseError(Unexpected(head)))
      } yield head
    case Or(parser1, parser2) =>
      M.get.flatMap { input =>
        M.handleError(run(parser1)) { _ =>
          M.put(input) *> run(parser2)
        }
      }
    case And(parserA, parserB) =>
      for {
        a <- run(parserA)
        b <- run(parserB)
      } yield (a, b)
  }
```
---

```tut:silent
type Result[A] = Either[Error, A]
type ResultST[A]  = StateT[Result, String, A]

val toResultST: Parser ~> ResultST = new (Parser ~> ResultST) {
  val MS: MonadState[ResultST, String] =
    StateT.stateTMonadState[String, Result]
  val ME: MonadError[ResultST, Error] =
    StateT.stateTMonadError[String, Result, Error]
  def apply[A](parser: Parser[A]): ResultST[A] =
    run(parser)(MonadStateError(MS, ME))
}

def eval[A](parser: Parser[A])(input: String): Result[A] =
  toResultST(parser).eval(input)
```

--

```tut
val result = toResultST(aAndB)
```
--

```tut
result.run("")
result.run("z")
result.run("abcd")
result.eval("abcd")
```

---

```tut:silent
sealed trait Parser[A]
case class Exactly(char: Char) extends Parser[Char]
case class Pure[A](value: A) extends Parser[A]
case class Or[A](
  parser1: Parser[A],
  parser2: Parser[A]
) extends Parser[A]
case class Bind[A, B](
  parser: Parser[A],
  f: A => Parser[B]
) extends Parser[B]
```

--

```tut:silent
implicit val parserMonad: Monad[Parser] = new Monad[Parser] {
  def point[A](a: => A): Parser[A] = Pure(a)
  def bind[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = Bind(fa, f)
}
```

--

```tut:silent
implicit val parserPlus: Plus[Parser] = new Plus[Parser] {
  def plus[A](a: Parser[A], b: => Parser[A]): Parser[A] = Or(a, b)
}
```

--

```tut:silent
implicit def parserSemigroup[A]: Semigroup[Parser[A]] =
  parserPlus.semigroup[A]
```

---

```tut:silent
def and[A, B](parserA: Parser[A], parserB: Parser[B]): Parser[(A, B)] =
  parserA.tuple(parserB)
```
--

```tut:silent
val allDigits: NonEmptyList[Char] = NonEmptyList('0', ('1' to '9'): _*)

def charToInt(c: Char): Int = c.toString.toInt // ¯\_(ツ)_/¯

val digit: Parser[Int] = allDigits.foldMap1(d => Exactly(d).map(charToInt))
```
--

```tut:silent
def maybe[A](parser: Parser[A]): Parser[Option[A]] =
  Or(parser.map(Option(_)), Pure(Option.empty[A]))
```
--

```tut:silent
object someMany {
  def some[A](parser: Parser[A]): Parser[List[A]] =
    for {
      head <- parser
      tail <- many(parser)
    } yield head :: tail

  def many[A](parser: Parser[A]): Parser[List[A]] =
    Or(some(parser), Pure(List.empty[A]))
}

import someMany._
```
---

```tut:silent
def run[F[_], A](
  parser: Parser[A]
)(
  implicit M: MonadStateError[F, String, Error]
): F[A] =
  parser match {
    case Pure(value) => M.point(value)
    case Exactly(char) =>
      for {
        head <- next
        _ <- whenM(head != char)(M.raiseError(Unexpected(head)))
      } yield head
    case Or(parser1, parser2) =>
      M.get.flatMap { input =>
        M.handleError(run(parser1)) { _ =>
          M.put(input) *> run(parser2)
        }
      }
    case Bind(parser, f) => run(parser).flatMap(a => run(f(a)))
  }
```
--

```tut:silent
type Result[A] = Either[Error, A]
type ResultST[A]  = StateT[Result, String, A]
val toResultST: Parser ~> ResultST = new (Parser ~> ResultST) {
  val MS: MonadState[ResultST, String] = StateT.stateTMonadState[String, Result]
  val ME: MonadError[ResultST, Error] = StateT.stateTMonadError[String, Result, Error]
  def apply[A](parser: Parser[A]): ResultST[A] = run(parser)(MonadStateError(MS, ME))
}
def eval[A](parser: Parser[A])(input: String): Result[A] = toResultST(parser).eval(input)
```

---
```tut:silent
val lowercase: Parser[Char] =
  NonEmptyList('a', ('b' to 'z'): _*).foldMap1[Parser[Char]](Exactly(_))
```
--

```tut:silent
val foo: Parser[(Char, List[Int])] = and(lowercase, some(digit))
```
--

```tut:silent
val result = toResultST(foo)
```
--

```tut
result.run("")
result.run("A")
result.run("a")
result.run("b42az")
```
---

```tut:silent
val protocol: Parser[String] =
  for {
    n     <- digit
    _     <- Exactly(':')
    chars <- lowercase.replicateM(n)
  } yield chars.mkString
```
--

```tut
eval(protocol)("")
eval(protocol)("abcdefg42")
eval(protocol)("3abcdefg42")
eval(protocol)("3:abcdefg42")
```
--

#🍾

