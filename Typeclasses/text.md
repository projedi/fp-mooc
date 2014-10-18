---
vim: ft=markdown
---

What I know about typeclasses
=============================

* Что такое параметрический и ad hoc полиморфизм:
    - Parametric: Works the same way for every single type
    - Ad hoc: For every type there is a special implementation
* Как ad hoc работает в Haskell:
    - Typeclasses as a number of methods
    - Class in a more mathematical sense
* Чем похоже, чем отличается от интерфейсов в Java
    - In Java an overloaded method is resolved based on its arguments
      (an implicit `this` argument)
    - In Haskell it is inferred from the complete type signature
* Стандартные классы типов:
    - Eq (no built-in equalities)
    - Show (yet another derivable instance)
    - Ord (inheritance)
    - Num (polymorphic number literals)
* Тонкости:
    - Deriving + standalone deriving
    - Multi-parameter + fundeps
    - Orphan instances (and how to segfault with 'em)
    - Overlapping + undecidable instances
    - Type synonym instances
    - Flexible instances & contexts
* Implementation of typeclasses. How do they look in Core:
    - Dictionary passing

Уроки
=====

Классы типов
------------

### Step 1 (Parametric polymorphism)

Полиморфизм - механизм, позволяющий оперировать с данными
разного типа используя единый интерфейс.

Рассмотрим, к примеру:

    map :: (a -> b) -> [a] -> [b]
    map f [] = []
    map f (x :: xs) = f x :: map f xs

Функцию `map` можно применить к аргументам с типами `Int -> Int`, `[Int]`:

    map (*2) [1, 2, 3] = [2, 4, 6]

и к аргументам с типами `Double -> Integer`, `[Double]`:

    map round [1.41, 2.71, 3.14] = [1, 3, 3]

При этом, код `map` используется один и тот же. Это называется
**"параметрический полиморфизм"**. Он нам дает некоторые гарантии
на поведение функции. Типы гарантируют, что `map` может только:

* всегда возвращать пустой список,
* на основании индексов переупорядочить элементы второго аргумента,
* на основании индексов выбрать лишь некоторые элементы второго аргумента,
* на основании индексов клонировать некоторые элементы второго аргумента,
* применить к каждому элементу второго аргумента (возможно измененного предыдущими
  тремя правилами) первый аргумент и вернуть полученный список.

### Step 2 (Ad hoc polymorphism)

Рассмотрим теперь равенство `(==) :: a -> a -> Bool`. Согласно параметрическому
полиморфизму оно должно работать одинаково как для примитивных типов
`Char#`, `Int#`, ... [^1], так для алгебраических типов `[a]`, `Maybe a`, ...,
так и для функций `Int -> Int`, `a -> b`, .... Соответственно, 2 выхода:

* Запретить использование примитивных типов и все сравнивать по указателям.
* Сделать реализацию `(==)` уникальной для каждого типа.

Второй способ называется **"ad hoc полиморфизм"**. В данном случае он
подходит лучше, потому что мы можем определить равенство для примитивных
типов, задать равенство для алгебраических типов как равенство значений и
не задавать равенство на функциях вообще. Действительно, пусть у нас есть 2
функции вычисления цифры с номером `n` в числе π:

    piDigit1 :: Integer -> Int
    piDigit1 n = ...

    piDigit2 :: Integer -> Int
    piDigit2 n = ...

В таком случае равенство `piDigit1 == piDigit2` по значениям неразрешимо (множество
значений первого аргумента бесконечно). Разрешима только проверка, что функция слева
ссылается на ту же реализацию, что и функция справа. А эта информация на практике
бессмысленна.

[^1]: Типы `Char`, `Int` --- обертки над примитивными типами `Char#`, `Int#`.

### Step 3 (Typeclasses)

### Step 4 (Similarities with Java interfaces)

### Step 5 (Contexts)

Стандартные классы типов
------------------------

### Step 1 (Eq --- "No built-in equalities")

### Step 2 (Show --- "Yet another derivable instance")

### Step 3 (Ord --- "Inheritance")

### Step 4 (Num --- "Polymorphic number literals")

Advanced typeclassing
---------------------
