class: center, middle, hidden-slide-number
count: false

# Parser Combinators

### A _type-driven_ approach to input processiEOF
---
class: bio, hidden-slide-number
##Bastien Louërat

![beard](img/beard.gif)

Developer @ **Habito**

**Scala** background

Rambling **@blouerat**

_ctrl+u_ **github.com/blouerat**
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
class: background-cover, hidden-slide-number
background-image: url(img/utopya.jpg)

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
type ResultST[A] = StateT[Result, String, A]

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
    case Exactly(char) =>
      for {
        head <- next
        _ <- whenM(head != char)(M.raiseError(Unexpected(head)))
      } yield head
    case Pure(value) => M.point(value)
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
type ResultST[A] = StateT[Result, String, A]
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
---

#Parser combinators in Idris

```idris
data Grammar : (tok : Type) -> (consumes : Bool) -> Type -> Type where
     Empty : (val : ty) -> Grammar tok False ty
     Terminal : (tok -> Maybe a) -> Grammar tok True a
     NextIs : (tok -> Bool) -> Grammar tok False tok
     EOF : Grammar tok False ()

     Fail : String -> Grammar tok c ty
     Commit : Grammar tok False ()

     SeqEat : Grammar tok True a -> Inf (a -> Grammar tok c2 b) ->
              Grammar tok True b
     SeqEmpty : {c1, c2 : Bool} ->
                Grammar tok c1 a -> (a -> Grammar tok c2 b) ->
                Grammar tok (c1 || c2) b
     Alt : {c1, c2 : Bool} ->
           Grammar tok c1 ty -> Grammar tok c2 ty ->
           Grammar tok (c1 && c2) ty

(>>=) : {c1 : Bool} ->
        Grammar tok c1 a ->
        inf c1 (a -> Grammar tok c2 b) ->
        Grammar tok (c1 || c2) b
(>>=) {c1 = False} = SeqEmpty
(>>=) {c1 = True} = SeqEat
```

---

```tut:silent
sealed trait Bool {
  type If[T <: Out, F <: Out, Out] <: Out
}
sealed trait True extends Bool {
  type If[T <: Out, F <: Out, Out] = T
}
sealed trait False extends Bool {
  type If[T <: Out, F <: Out, Out] = F
}

type &&[A <: Bool, B <: Bool] = A#If[B, False, Bool]
type ||[A <: Bool, B <: Bool] = A#If[True, B, Bool]
```
--

```tut:silent
object Bool {
  sealed trait Refl[A <: Bool, B <: Bool]

  object Refl extends Refl0

  trait Refl0 extends Refl1 {
    implicit def reflAndTrue[A <: Bool]: Refl[A && True, A] = null
    implicit def reflAndFalse[A <: Bool]: Refl[A && False, False] = null
  }

  trait Refl1 {
    implicit def reflOrTrue[A <: Bool]: Refl[A || True, True] = null
    implicit def reflOrFalse[A <: Bool]: Refl[A || False, A] = null
  }
}

import Bool._
```
---

```tut:silent
sealed trait Parser[A, X <: Bool]
```
--

```tut:silent
case class Exactly(char: Char) extends Parser[Char, True]
```
--

```tut:silent
case class Pure[A](value: A) extends Parser[A, False]
```
--

```tut:silent
case class Or[A, X1 <: Bool, X2 <: Bool](
  parser1: Parser[A, X1],
  parser2: Parser[A, X2]
) extends Parser[A, X1 && X2]
```
--

```tut:silent
case class Bind[A, X1 <: Bool, B, X2 <: Bool](
  parser: Parser[A, X1],
  f: A => Parser[B, X2]
) extends Parser[B, X1 || X2]

```
---

```tut:silent
object Parser {
  sealed trait Parser[A, X <: Bool] { self =>
    def map[B](f: A => B): Parser[B, X] =
      Bind(self, (a: A) => Pure(f(a))).refl
    def flatMap[B, X2 <: Bool](f: A => Parser[B, X2]): Parser[B, X || X2] =
      Bind(self, f)
    def refl[X2 <: Bool](implicit refl: Refl[X, X2]): Parser[A, X2] =
      this.asInstanceOf[Parser[A, X2]]
  }

  case class Exactly(char: Char) extends Parser[Char, True]
  case class Pure[A](value: A) extends Parser[A, False]
  case class Or[A, X1 <: Bool, X2 <: Bool](
    parser1: Parser[A, X1],
    parser2: Parser[A, X2]
  ) extends Parser[A, X1 && X2]
  case class Bind[A, X1 <: Bool, B, X2 <: Bool](
    parser: Parser[A, X1],
    f: A => Parser[B, X2]
  ) extends Parser[B, X1 || X2]
}

import Parser._
```
---

```tut:silent
val a: Parser[Char, True] = Exactly('a')
val b: Parser[Char, True] = Exactly('b')
```
--

```tut:silent
val aOrB: Parser[Char, True] = Or(a, b)
```
--

```tut:silent
def wrapped[A, X <: Bool](parser: Parser[A, X]): Parser[A, True] =
  for {
    _ <- Exactly('(')
    a <- parser
    _ <- Exactly(')')
  } yield a
```
--

```tut:silent
def maybe[A, X <: Bool](parser: Parser[A, X]): Parser[Option[A], False] =
  Or(parser.map(Option(_)), Pure(Option.empty[A])).refl
```
--

```tut:silent
object someMany {
  def some[A, X <: Bool](parser: Parser[A, X]): Parser[List[A], X] =
    parser.flatMap { a =>
      many(parser).map(a :: _)
    }.refl

  def many[A, X <: Bool](parser: Parser[A, X]): Parser[List[A], False] =
    Or(some(parser), Pure(List.empty[A])).refl
}

import someMany._
```

---

```tut:silent
def runForget[F[_], A](
  parser: Parser[A, _]
)(
  implicit M: MonadStateError[F, String, Error]
): F[A] =
  parser match {
    case Exactly(char) =>
      for {
        head <- next
        _ <- whenM(head != char)(M.raiseError(Unexpected(head)))
      } yield head
    case Pure(value) => M.point(value)
    case Or(parser1, parser2) =>
      M.get.flatMap { input =>
        M.handleError(runForget(parser1)) { _ =>
          M.put(input) *> runForget(parser2)
        }
      }
    case Bind(parser, f) => runForget(parser).flatMap(a => runForget(f(a)))
  }
```
--

```tut:silent
type Result[A] = Either[Error, A]
type ResultST[A]  = StateT[Result, String, A]
def toResultST[A](parser: Parser[A, True]): ResultST[A] = {
  val MS: MonadState[ResultST, String] = StateT.stateTMonadState[String, Result]
  val ME: MonadError[ResultST, Error] = StateT.stateTMonadError[String, Result, Error]
  runForget(parser)(MonadStateError(MS, ME))
}
def eval[A](parser: Parser[A, True])(input: String): Result[A] = toResultST(parser).eval(input)
```
---

```tut:book
val aOrBs = some(wrapped(aOrB))
```
--

```tut
eval(aOrBs)("")
eval(aOrBs)("abcd")
```
--

```tut
eval(aOrBs)("(a)(b)(b)(a)cd")
```
--

```tut:book
val maybeAB = maybe(aOrB)
```
--

```tut:fail
eval(maybeAB)("")
```

---
```tut:silent
def stupid: Parser[Unit, False] = Pure(()).flatMap(_ => stupid)
```
--

```tut:silent
def woops: Parser[Unit, True] = Exactly('$').flatMap(_ => stupid)
```
--

```tut
eval(woops)("")
```
--

```tut:fail
eval(woops)("$")
```
---
class: middle, poem

![nein](img/nein.jpg)

###Cynicism: The hope that someday you will have known better all along.

Nein. A Manifesto, Eric Jarosinski
---
class: middle

#One must imagine Sisyphus happy

![Sisyphus comic](img/sisyphus_comic.png)

[Existential Comics](http://existentialcomics.com/comic/29)
---
class: black, center, middle, hidden-slide-number

# Thank you
---
###Reading list

* Functional Programming in Scala, _Paul Chiusano and Runar Bjarnason_
* Type-Driven Development with Idris, _Edwin Brady_
* Monadic parsing in Haskell, _Graham Hutton and Erik Meijer_
