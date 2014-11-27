<!--
vim: ft=markdown
-->

Уроки
=====

Классы типов
------------

### Step 1 (Implementing `isElemOf`)

Попробуем реализовать функцию, использующую равенство, --- `isElemOf`.

Ясно, что реализация равенства зависит от типа, элементы которого сравниваем,
поэтому нету функции на верхнем уровне вида `(==) :: a -> a -> Bool`. Тогда
можно попробовать передавать реализацию равенства аргументом:

    isElemOf :: (a -> a -> Bool) -> a -> [a] -> Bool
    isElemOf _ _ [] = False
    isElemOf eq x (y : ys) = x `eq` y || isElemOf eq x ys

### Step 2 (Exercise: implement `isSublistOf`)

Реализовать в терминах `isElemOf`:

    isSublistOf :: (a -> a -> Bool) -> [a] -> [a] -> Bool
    -- isSublistOf (==) [] [] == True
    -- isSublistOf (==) [] [1, 2, 3] == True
    -- isSublistOf (==) [2, 1] [1, 2, 3] == True
    -- isSublistOf (==) [2, 4] [1, 2, 3] == False

### Step 3 (Typeclasses)

А что если для нашей функции мы хотим передать не только реализию равенства,
а еще преобразование в строку и оператор `(<)`. Придется передавать 3 дополнительных
аргумента и при этом первый и третий легко перепутать, так как они имеют одинаковый
тип `a -> a -> Bool`. Haskell предоставляет механизм классов типов, который позволяет
автоматизировать этот процесс:

    class Eq a where
       (==) :: a -> a -> Bool

    class Ord a where
       (<) :: a -> a -> Bool

    class Show a where
       show :: a -> String

    sort :: (Ord a) => [a] -> [a]
    sort [] = []
    sort (x : xs) =
       let (ls, rs) = go xs
       in sort ls ++ [x] ++ sort rs
     where go [] = ([], [])
           go (y : ys) =
              let (ls, rs) = go ys
              in if y < x then (y : ls, rs) else (ls, y : rs)

    nubSorted :: (Eq a) => [a] -> [a]
    nubSorted [] = []
    nubSorted (x : xs) = x : nubSorted (go xs)
     where go [] = []
           go (y : ys)
            | x == y = go ys
            | otherwise = y : ys

    f :: (Eq a, Ord a, Show a) => [a] -> String
    f = show . nubSorted . sort

Мы декларируем функцию `(==)` в _классе_ `Eq`, а реализацию для типа
`a` неявно передаем из функции `f` в `nubSorted`, указывая `Eq a` в
_контексте_ (все, что между `::` и `=>`). Для `(<)` мы передаем из
`f` в `sort`, используя `Ord`. А `show` мы используем в самой `f`,
беря реализацию из контекста.

`isElemOf` теперь принимает вид:

    isElemOf :: (Eq a) => a -> [a] -> Bool
    isElemOf _ [] = False
    isElemOf x (y : ys) = x == y || x `isElemOf` ys

### Step 4 (Exercise: implement `isSublistOf` again)

Реализовать в терминах `isElemOf`:

    isSublistOf :: (Eq a) => [a] -> [a] -> Bool
    -- isSublistOf [] [] == True
    -- isSublistOf [] [1, 2, 3] == True
    -- isSublistOf [2, 1] [1, 2, 3] == True
    -- isSublistOf [2, 4] [1, 2, 3] == False

### Step 5 (Typeclass instances)

Теперь, наладив неявную передачу, необходимо откуда-то взять саму реализацию.
Вспомним, что мы начинали с проблемы, что равенство необходимо определять для
каждого типа по-своему. Что и сделаем, синтаксически это выглядит так:

    instance Eq Int where
       (I# i1) == (I# i2) = i1 ==# i2

    instance (Eq a) => Eq [a] where
       [] == [] = True
       (x : xs) == (y : ys) = x == y && xs == ys
       _ == _ = False

Мы определяем эти _инстансы_ на верхнем уровне и требуем, чтобы для каждого типа
было не больше одного инстанса. Здесь `(==#)` --- встроенный оператор равенства для
типа `Int#`. Во втором клозе `(==)` для списков в `x == y` используется равенство
из `Eq a`, а в `xs == ys` используется равенство из `Eq [a]` (рекурсивный вызов).

### Step 6 (Exercise: Implement Eq instance for binary trees)

Реализовать инстанс `Eq` для дерева:

    data Tree a
       = Leaf a
       | Branch (Tree a) a (Tree a)

### Step 7 (More on typeclasses)

Но нужно ли ограничиваться одной функцией для класса типов? Нет:

    class Eq a where
       (==) :: a -> a -> Bool
       (/=) :: a -> a -> Bool
       x /= y = not (x == y)

    class (Eq a) => Ord a where
       compare :: a -> a -> Ordering
       compare x y
        | x == y = EQ
        | x <= y = LT
        | otherwise = GT

       (<), (<=), (>), (>=) :: a -> a -> Bool
       x < y =
          case compare x y of
             LT -> True
             _ -> False
       x <= y =
          case compare x y of
             GT -> False
             _ -> True
       x > y =
          case compare x y of
             GT -> True
             _ -> False
       x >= y =
          case compare x y of
             LT -> False
             _ -> True

       max, min :: a -> a -> a
       max x y = if x <= y then y else x
       min x y = if x <= y then x else y

    instance Eq Int where
       (I# i1) == (I# i2) = i1 ==# i2
       (I# i1) /= (I# i2) = i1 /=# i2

    instance Eq a => Eq [a] where
       [] == [] = True
       (x : xs) == (y : ys) = x == y && xs == ys
       _ == _ = False

    instance (Ord a) => Ord [a] where
       compare [] [] = EQ
       compare [] (_ : _) = LT
       compare (_ : _) [] = GT
       compare (x : xs) (y : ys) =
          case compare x y of
             EQ -> compare xs ys
             other -> other

    instance Ord Int where
       (I# x#) `compare` (I# y#)
        | isTrue# (x# <# y#) = LT
        | isTrue# (x# ==# y#) = EQ
        | otherwise = GT
       (I# x) < (I# y) = isTrue# (x <# y)
       (I# x) <= (I# y) = isTrue# (x <=# y)
       (I# x) >= (I# y) = isTrue# (x >=# y)
       (I# x) > (I# y) = isTrue# (x ># y)

Здесь в `Eq` определены 2 функции `(==)`, `(/=)` и у `(/=)` есть реализация
по умолчанию. В инстансе для списка используется именно она, а в инстансе для
`Int` она переопределена в интересах эффективности.

`Ord` интереснее: во-первых, определяя инстанс для `Ord`, необходимо так же определять
инстанс `Eq` (присутствие контекста `(Eq a) =>` при определении `Ord`). Во-вторых,
каждая функция имеет реализацию по-умолчанию. При этом, `compare` определен через `(<=)`
и `(<=)` определен через `compare`. Поэтому, чтобы программа не ушла в бесконечный цикл,
нужно определить хотя бы один из них. Обычно определяют `compare` в интересах эффективности.

### Step 8 (Exercise: More complex typeclasses somehow)

### Step 9 (Philosophical concepts and whatnot)

#### Математическая интерпретация классов

Если рассматривать типы как множества, то классы типов можно понимать в математическом
смысле: совокупность множеств, объединенная некоторым признаком.

В случае `Eq` мы определяем совокупность множеств, таких что для каждого множества
`a` определена операция `(==) :: a -> a -> Bool` и операция `(/=) :: a -> a -> Bool`.

В случае `Ord` мы определяем класс множеств, с операциями `compare`, `(<)`, ... и
при условии, что множества из этого класса так же принадлежит классу `Eq`. Инстансы
тогда интерпретируются как свидетели принадлежности множества классу.

#### Сходства с Java интерфейсами

Если смотреть на классы типов со стороны ООП, то они напоминают
интерфейсы в Java. Но есть отличие: в Java конкретная реализация
выбирается по неявному аргументу `this`, а в Haskell
участвует вся сигнатура функции:

    class C a where
       m1 :: a -> Integer
       m2 :: Integer -> a
       m3 :: [a] -> Integer

    instance C Int where
       m1 x = ...
       m2 i = ...
       m3 xs = ...

В `f1`, `f2`, `f3` реализации `m1`, `m2`, `m3` выбираются из инстанса `C Int`:

    a1, a2, a3 :: Int
    a1 = 1
    a2 = 2
    a3 = 3

    f1 :: Integer
    f1 = m1 a1

    f2 :: Int
    f2 = m2 0

    f3 :: Integer
    f3 = m3 [a1, a2, a3]

Стандартные классы типов
------------------------

### Step 1 (Eq --- "No built-in equalities")

### Step 2 (Show --- "Yet another derivable instance")

### Step 3 (Ord --- "Inheritance")

### Step 4 (Num --- "Polymorphic number literals")

Advanced typeclassing
---------------------

TODO
====

* Deriving + standalone deriving
* Multi-parameter + fundeps
* Orphan instances (and how to segfault with 'em)
* Overlapping + undecidable instances
* Type synonym instances
* Flexible instances & contexts
* Implementation of typeclasses. How do they look in Core.
