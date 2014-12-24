<!--
vim: ft=markdown
-->

Уроки
=====

Введение в классы типов
-----------------------

### Step 1 (Implementing `isElemOf`)

Попробуем реализовать функцию, использующую равенство, --- `isElemOf`.

Ясно, что реализация равенства зависит от типа, элементы которого сравниваем,
поэтому нету функции на верхнем уровне вида `(==) :: a -> a -> Bool`.
Можно попробовать передавать реализацию равенства аргументом:

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

А что если для нашей функции мы хотим передать не только реализацию равенства,
а еще преобразование в строку и оператор `(<)`? Придется передавать 3 дополнительных
аргумента и при этом первый и третий легко перепутать, так как они имеют одинаковый
тип `a -> a -> Bool`. Haskell предоставляет механизм классов типов, который позволяет
автоматизировать процесс:

    class Eq a where
       (==) :: a -> a -> Bool

Здесь мы декларируем функцию `(==)` в *классе* `Eq`.  `isElemOf` теперь можно
переписать так:

    isElemOf :: (Eq a) => a -> [a] -> Bool
    isElemOf _ [] = False
    isElemOf x (y : ys) = x == y || x `isElemOf` ys

В сигнатуре функции все, что между `::` и `=>` называется
*контекстом*. В данном случае он состоит только из `Eq a` и
используется в качестве неявного аргумента для передачи функции `(==)`.

### Step 4 (Exercise: implement `isSublistOf` again)

Реализовать в терминах `isElemOf`:

    isSublistOf :: (Eq a) => [a] -> [a] -> Bool
    -- isSublistOf [] [] == True
    -- isSublistOf [] [1, 2, 3] == True
    -- isSublistOf [2, 1] [1, 2, 3] == True
    -- isSublistOf [2, 4] [1, 2, 3] == False

### Step 5 (Multiple constraints in a context)

Введем еще классы для оператора `(<)` и для преобразования
в строку:

    class Ord a where
       (<) :: a -> a -> Bool

    class Show a where
       show :: a -> String

Теперь посмотрим на передачу `(==)`, `(<)` и `show` через контекст:

    import Data.List (nub, sort)

    -- sort :: (Ord a) => [a] -> [a]
    -- nub :: (Eq a) => [a] -> [a]

    f :: (Eq a, Ord a, Show a) => [a] -> String
    f = show . nub . sort

В `f` контекст состоит из `(Eq a, Ord a, Show a)`.
`Eq a` неявно передается в `nub`, `Ord a` --- в `sort`, `Show a` используется
только в `f` при вызове `show`.

### Step 6 (Parameters in a class)

    data T a = T a

    f :: (Eq (T a), Eq b, Eq a) => a -> T a -> b
    f = ...

У функции `f` контекст дает три неявных аргумента с функциями:

    (==) :: T a -> T a -> Bool
    (==) :: b -> b -> Bool
    (==) :: a -> a -> Bool

В теле `f` при встрече `(==)` будет выбрана нужная реализация в зависимости от типа аргументов.

### Step 7 (Typeclass instances)

Теперь, наладив неявную передачу, необходимо откуда-то взять саму реализацию.
Как уже говорилось, равенство необходимо определять для каждого типа по-своему,
что и сделаем. Синтаксически это выглядит так:

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

### Step 8 (Exercise: Implement Eq instance for binary trees)

Реализовать инстанс `Eq` для дерева:

    data Tree a
       = Leaf a
       | Branch (Tree a) a (Tree a)

### Step 9 (More on typeclasses)

Но нужно ли ограничиваться одной функцией для класса типов? Нет,
вот как определены классы `Eq` и `Ord` на самом деле:

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
нужно определить хотя бы один из них. Обычно определяют либо `compare`, либо все кроме
`max`, `min`.

### Step 10 (Exercise: implement `MapLike`)

Определить класс `MapLike` с методами `empty`, `lookup`, `insert`, `delete`, `fromList`.
Они должны иметь ту же семантику, что и функции с этими именами в модуле `Data.Map`.
У `fromList` должна быть реализация по умолчанию. Определить инстансы для `Data.Map.Map` и для `ListMap`:

    newtype ListMap k v = ListMap [(k, v)]

### Step 11 (Philosophy)

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

Классы типов Show и Read
-----------------------

### Step 1 (Show)

Для преобразования в строку в Haskell есть класс типов `Show`:
```
class Show a where
   show :: a -> String
```
Он в том числе используется ghci, чтобы напечатать результат.
Вот пример реализации инстанса для бинарного дерева:
```
data Tree a
   = Leaf a
   | Branch (Tree a) a (Tree a)

instance Show a => Show (Tree a) where
   show (Leaf x) = show x
   show (Branch l x r) = "(" ++ show l ++ ") " ++ show x ++ " (" ++ show r ++ ")"
```

### Step 2 (Exercise: Implement Show instance for binary trees by drawing)

Реализовать инстанс `Show` для деревьев, который бы "рисовал" деревья:
```
data Tree a
   = Leaf a
   | Branch (Tree a) a (Tree a)

-- show (Leaf 0) == "0\n"
-- show (Branch (Leaf 0) 1 (Leaf 2)) == " 1 \n\
--                                      \0 2\n"
-- show (Branch (Branch (Leaf 0) 24 (Leaf 22)) 45 (Branch (Branch (Leaf 34) 37 (Leaf 8)) 100 (Leaf 1))) ==
--    "     45         \n\
--    \ 24         100 \n\
--    \0  22    37    1\n\
--    \       34  8    \n\
```
*Подсказка*: Напишите вспомогательную функцию, которая строит отрисованное дерево построчно (так проще
склеивать два поддерева при обработке `Branch`). А `show` просто вызовет эту функцию и применит к результату `unlines`.

### Step 3 (Show is slow)

Посмотрим теперь на производительность `show`:
```
data Nat = S Nat | Z

instance Show Nat where
   show Z = "0"
   show (S n) = show n ++ "+1"

-- show (S (S Z)) =>
-- show (S Z) ++ "+1" =>
-- (show Z ++ "+1") ++ "+1" =>
-- ("0" ++ "+1") ++ "+1"
```
Мы получили последовательность скобок, при которой время работы `(++)` наихудшее.
Попробуем исправить ситуацию:
```
type ShowS = String -> String

showNat :: Nat -> ShowS
showNat Z = ("0" ++)
showNat (S n) = showNat n . ("+1" ++)

instance Show Nat where
   show n = showNat n ""

-- show (S (S Z)) =>
-- showNat (S (S Z)) "" =>
-- (showNat (S Z) . ("+1" ++)) "" =>
-- (\x -> showNat (S Z) ("+1" ++ x)) "" =>
-- (\x -> (showNat Z . ("+1" ++)) ("+1" ++ x)) "" =>
-- (\x -> (\y -> showNat Z ("+1" ++ y)) ("+1" ++ x)) "" =>
-- (\x -> (\y -> "0" ++ ("+1" ++ y)) ("+1" ++ x)) "" =>
-- (\x -> "0" ++ ("+1" ++ ("+1" ++ x))) "" =>
-- "0" ++ ("+1" ++ ("+1" ++ ""))
```
Использование композиции для конкатенации строк исправило ситуацию.

### Step 4 (Show again)

Этот трюк на самом деле используется и в самом `Show`. Вот как класс выглядит на самом деле:
```
class Show a where
   showPrec :: Int -> a -> ShowS
   show :: a -> String
   showList :: [a] -> ShowS
```
Достаточно реализовать `showPrec` *или* `show`.
`showList` позволяет переопределить, как будет показываться список элементов (используется в
`Show Char` для обработки `String`).
`showPrec` первым аргументом принимает приоритет оператора в окружающем контексте (от 0 до 11) ---
используется, например, при выводе инфиксных операторов.

Также есть набор вспомогательных функций:
```
shows :: (Show a) => a -> ShowS
shows = showPrec 0

showChar :: Char -> ShowS
showChar = (:)

showString :: String -> ShowS
showString = (++)

showParen :: Bool -> ShowS -> ShowS
showParen b p = if b then showChar '(' . p . showChar ')' else p
```

### Step 5 (Exercise: Implement Show instance for expressions)

Реализовать инстанс `Show` для арифметических выражений:
```
data Expr
   = Val Int
   | Sum Expr Expr
   | Mul Expr Expr

-- show (Sum (Mul (Val 1) (Val 2)) (Mul (Val 3) (Val 4))) == "1 * 2 + 3 * 4"
-- show (Mul (Sum (Val 1) (Val 2)) (Sum (Val 3) (Val 4))) == "(1 + 2) * (3 + 4)"
-- show (Sum (Val 1) (Sum (Val 2) (Val 3))) == "1 + 2 + 3"
-- show (Mul (Mul (Val 1) (Val 2)) (Val 3)) == "1 * 2 * 3"
```

### Step 6 (Read)

Помимо преобразования в строку в Haskell есть так же класс для преобразования из строки, `Read`.
Вот он, как обычно, в своей неполноте вместе со вспомогательными функциями:
```
type ReadS a = String -> [(a, String)]

class Read a where
   readsPrec :: Int -> ReadS a
   readList :: ReadS [a]

reads :: Read a => ReadS a
read :: Read a => String -> a
readParen :: Bool -> ReadS a -> ReadS a
lex :: ReadS String
```
Достаточно определить `readsPrec`.

* `ReadS a` --- парсер для `a`: принимает на вход строку и возвращает список возможных результатов
  `(a, String)`, где `String` --- остаток строки, не участвовший в парсинге.
* `readsPrec` --- так же, как и `showPrec` принимает первым аргументом приоритет оператора
  в окружающем контексте.
* `readList` --- так же, как и `showList` используется для особой обработки парсинга списков.
  И используется в инстансе `Read Char`.
* `reads` --- эквивалентно `readsPrec 0`.
* `read` --- попытается *полностью* распарсить вход и упадет с ошибкой при неудаче.
* `readParen` --- `readParen True p` парсит `p`, заключенное в скобки;
  `readParen False p` парсит `p`, *возможно* заключенное в скобки.
* `lex` --- прочитать одну лексему, пропустив лидирующие пробелы. Лексема:
    - `'a'` --- символьный литерал
    - `"abc"` --- строковый литерал
    - `foo123'` --- идентификатор в Haskell
    - `<<`, `:`, ... --- оператор в Haskell
    - `(`, `::`, ... --- пунктуация и зарезервированные символы в Haskell
    - `12.3e-45` --- число

### Step 7 (Exercise: Implement Show and Read instances for binary trees via Prüfer coding)

### Step 8 (ReadPrec (w/o delving into parser combinators))

Класс типов Num и его наследники
--------------------------------

### Step 1 (Num)

### Step 2 (Real)

### Step 3 (Integral)

### Step 4 (Fractional)

### Step 5 (Floating)

### Step 6 (RealFrac)

### Step 7 (RealFloat)

Другие стандартные классы типов
-------------------------------

### Step 1 (Enum)

### Step 2 (Bounded)

### Step 2.5 (Exercise: Deriving Enum, Bounded into Cycle with safeSuc, safePred)

### Step 3 (Ix)

### Step 4 (Foldable)

### Step 5 (IsString)

Расширения классов типов и производные представители
----------------------------------------------------

### Step 1 (deriving, standalone deriving)

### Step 2 (Type synonym instances)

### Step 3 (Orphan instances)

### Step 4 (Multiple parameters)

### Step 5 (Functional dependencies)

### Step 6 (Associated type families (putting fun into functional dependencies))

### Step 7 (Flexible contexts and instances)

### Step 8 (Overlapping instances)

### Step 9 (Undecidable instances)
