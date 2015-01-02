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
Можете проверить, что если showNat выглядел бы так:
```
showNat :: Nat -> ShowS
showNat Z = ("0" ++)
showNat (S n) = ("1+" ++) . showNat n
```
то последовательность скобок осталась бы эффективной.

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
Вот, как обычно, его часть вместе со вспомогательными функциями:
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

### Step 7 (Read example)

В качестве примера, рассмотрим парсинг времени поддерживающий как "22:34", так и "10:34PM":
```
data Time = Time
   { hours :: Int
   , minutes :: Int
   }
   deriving Show

data DayPeriod = AM | PM

instance Read DayPeriod where
   readsPrec p = go
    where go (' ':s) = go s
          go ('A':'M':s) = [(AM, s)]
          go ('P':'M':s) = [(PM, s)]
          go _ = []

instance Read Time where
   readsPrec p s =
      [ (t, s'')
      | ((h, m), s') <- hoursMinutes s
      , (dp, s'') <- readsPrec p s'
      , t <- case dp of
                AM -> [ Time (h `rem` 12) m | h >= 1 && h <= 12 ]
                PM -> [ Time (h `rem` 12 + 12) m | h >= 1 && h <= 12 ]
      ] ++
      [ (Time h m, s')
      | ((h, m), s') <- hoursMinutes s
      , h >= 0 && h < 24
      ]
    where
      hoursMinutes :: ReadS (Int, Int)
      hoursMinutes s =
         [ ((h, m), s'')
         | (h, (':' : s')) <- readsPrec p s
         , (m, s'') <- readsPrec p s'
         , m >= 0 && m < 60
         ]
```
Если это выглядит не очень элегантно, то все как надо. Инструменты, которые позволят писать приличный
код для парсинга будут рассмотрены в последних двух модулях этого курса.

Инстанс для `DayPeriod` смотрит на входную строку, пропускает лидирующие пробельные символы и, если
строка начинается с `"AM"` или `"PM"`, возвращает соответственно `AM` или `PM` вместе с остатком строки.
Иначе возвращает пустой список результатов.

Инстанс для `Time` использует генераторы списков возможно неожиданным способом. Идея в том, чтобы рассматривать
функции, возвращающие списки, как недетерминированные вычисления: списком на выходе мы возвращаем все
возможные результаты этого вычисления (которых может быть любое количество, в том числе 0). В этом случае под
```
[ y | y <- f, y >= 0] ++ [ y | x <- f, y <- g x, y >= 0 ]
```
понимается недетрминированное вычисление, возвращающее либо неотрицательное значениe `f`, либо неотрицательный
результат применения `f` к `g`.

Посмотрим теперь на парсинг `Time`. Подпарсер `hoursMinutes` можно читать так:

1. Читаем `Int`, за которым следует `:` и храним этот `Int` в `h`.
2. Читаем `Int` и храним его в `m`.
3. Требуем `m >= 0 && m < 60`.
4. Возвращаем пару `(h, m)`.

Сам парсер состоит из 2-х частей. Пытаемся распарсить вход как:

* либо с наличием AM/PM:
    1. Читаем `(h, m)` с помощью `hoursMinutes`.
    2. Читаем `DayPeriod` и храним его в `dp`.
    3. В зависимости от `dp` ставим ограничения на `h` и конструируем `Time` из `h` и `m`.
* либо без:
    1. Читаем `(h, m)` с помощью `hoursMinutes`.
    2. Требуем `h >= 0 && h < 24`.
    3. Возвращаем `Time h m`.

Вызов `read s` для парсинга `Time` тогда будет смотреть на результат `readPrec 0 s`:

* Если есть ровно один результат, с пустой оставшейся строкой, то он вернет его.
* Если есть несколько таких результатов, то упадет, сказав, что парсинг недетерминированный.
* Если нет ни одного результата, либо все результаты имеют какую-то оставшуюся строку, то
  упадет, сказав, что парсинг неудачный.

### Step 8 (Exercise: Implement Show and Read instances to transform expression to PN and back)

Польская запись --- запись выражения в префиксной форме (оператор следует перед своими
аргументами). Например, "+ * 1 2 * 3 4" в инфиксной форме выглядит как "1 * 2 + 3 * 4".

Реализовать инстансы `Show` и `Read` для преобразования выражения в польскую запись
и обратно:
```
data Expr
   = Val Int
   | Sum Expr Expr
   | Mul Expr Expr

-- show (Sum (Mul (Val 1) (Val 2)) (Mul (Val 3) (Val 4))) == "+ * 1 2 * 3 4"
-- read " + * 1 2 3  " == Sum (Mul (Val 1) (Val 2)) (Val 3)
```

Класс типов Num и его наследники
--------------------------------

### Step 1 (Num)

Для работы с числами в Haskell есть целая иерархия классов типов. Самый базовый из них `Num`:
```
class Num a where
   (+), (-), (*) :: a -> a -> a
   negate :: a -> a
   abs :: a -> a
   signum :: a -> a
   fromInteger :: Integer -> a

   x - y = x + negate y
   negate x = 0 - x
```

* `(+)`, `(-)`, `(*)` --- обычные операторы сложения, вычитания и умножения.
* `negate` --- унарный минус.
* `abs` --- модуль числа.
* `signum` --- знак числа, должен соблюдаться закон `abs x * signum x == x`.
* `fromInteger` --- построение числа из `Integer`.

`fromInteger` используется при обработке численных литералов: когда мы пишем `3`,
компилятор переписывает это как применение `fromInteger` к значению типа `Integer`,
отвечающему тройке. Именно поэтому верно `3 :: Num a => a`.

### Step 2 (Real)

Вспомогательный класс, служащий индикатором, что тип является числом, то есть
может быть преобразован в `Rational` (рациональная дробь с числителем и знаменателем
имеющим тип `Integer`):
```
class (Num a, Ord a) => Real a where
   toRational :: a -> Rational
```

### Step 3 (Integral)

Для работы с целыми числами (в основном, целочисленным делением) есть класс `Integral`:
```
class (Real a, Enum a) => Integral a where
   quot :: a -> a -> a
   rem :: a -> a -> a
   div :: a -> a -> a
   mod :: a -> a -> a
   quotRem :: a -> a -> (a, a)
   divMod :: a -> a -> (a, a)
   toInteger :: a -> Integer

   n `quot` d = fst $ quotRem n d
   n `rem` d = snd $ quotRem n d
   n `div` d = fst $ divMod n d
   n `mod` d = snd $ divMod n d
   divMod n d = if signum r == negate (signum d) then (q - 1, r + d) else qr
    where qr@(q, r) = quotRem n d
```

* `quot` --- целочисленное деление, округленное в сторону нуля.
* `rem` --- остаток от деления, удовлетворяющий ``(x `quot` y) * y + (x `rem` y) == x``
* `div` --- целочисленное деление, округленное вниз.
* `mod` --- остаток от деления, удовлетворяющий ``(x `div` y) * y + (x `mod` y) == x``

`quot` и `rem` работают быстрее, чем `div` и `mod`, но на отрицательных числах выдают
другие результаты (например, ``(-1) `rem` 2 == -1``, а не соответсвующее математическому
определению остатка ``(-1) `mod` 2 == 1``). Но если алгоритм использет много арифметики
и заранее известно, что числа будут положительны, то использование `rem` даст значительный
прирост скорости
([пример](http://stackoverflow.com/questions/6964392/speed-comparison-with-project-euler-c-vs-python-vs-erlang-vs-haskell/6964760#6964760)).

`Enum` просто говорит, что можно перечислять элементы этого типа и будет рассмотрен в следующем уроке.

TODO: Mention `fromIntegral`

### Step 4 (Fractional)

Деление поддерживается классом `Fractional`:
```
class Num a => Fractional a where
   (/) :: a -> a -> a
   recip :: a -> a
   fromRational :: Rational -> a

   recip = 1 / x
   x / y = x * recip y
```

Заметьте, что класс не наследуется от `Real`, потому что, например, комплексные
числа могут быть поделены друг на друга, но к вещественным числам не приводятся.

Также в этом классе объявлена функция `fromRational`, которая используется для литералов
чисел с плавающей точкой. Например, `3.2e-3` представляется как значение, соответствующее
дроби `32 / 10000` типа `Rational`, примененяемое к `fromRational`. Поэтому,
`3.2e-3 :: Fractional a => a`.

TODO: Mention `realToFrac`

### Step 5 (Floating)

Поскольку иррациональные числа невозможно в точности представить на компьютере, а заставлять
хорошие (в плане корректности, а не производительности) нецелые числа вроде `Rational` нарушать
свои гарантии точности не хочется, то для тригонометрических функций, логарифмов и возведений
в дробные степени был введен свой класс `Floating`:
```
class Fractional a => Floating a where
   pi :: a
   exp, log, sqrt :: a -> a
   (**), logBase :: a -> a -> a
   sin, cos, tan :: a -> a
   asin, acos, atan :: a -> a
   sinh, cosh, tanh :: a -> a
   asinh, acosh, atanh :: a -> a

   x ** y = exp (log x * y)
   logBase x y = log y / log x
   sqrt x = x ** 0.5
   tan x = sin x / cos x
   tanh x = sinh x / cosh x
```

Он опять же не наследуется от `Real`, потому что эти операции имеют смысл для комплексных чисел.

### Step 6 (RealFrac)

Теперь функции округления вещественных чисел:
```
class (Real a, Fractional a) => RealFrac a where
   properFraction :: (Integral b) => a -> (b, a)
   truncate :: (Integral b) => a -> b
   round :: (Integral b) => a -> b
   ceiling :: (Integral b) => a -> b
   floor :: (Integral b) => a -> b

   truncate x = fst $ properFraction x
   round x = let (n, r) = properFraction x
                 m = if r < 0 then n - 1 else n + 1
             in case signum (abs r - 0.5) of
                   -1 -> n
                   0 -> if even n then n else m
                   1 -> m
                   _ -> error "Bad value"
   ceiling x = if r > 0 then n + 1 else n
    where (n, r) = properFraction x
   floor x = if r < 0 then n - 1 else n
    where (n, r) = properFraction x
```

`properFraction` --- поделить число на целую часть и правильную дробь (числитель строго меньше
знаменателя).

### Step 7 (RealFloat)

```
class (RealFrac a, Floating a) => RealFloat a where
   floatRadix :: a -> Integer
   floatDigits :: a -> Int
   floatRange :: a -> (Int, Int)
   decodeFloat :: a -> (Integer, Int)
   encodeFloat :: Integer -> Int -> a
   exponent :: a -> Int
   significand :: a -> a
   scaleFloat :: Int -> a -> a
   isNaN :: a -> Bool
   isInfinite :: a -> Bool
   isDenormalized :: a -> Bool
   isNegativeZero :: a -> Bool
   isIEEE :: a -> Bool
   atan2 :: a -> a -> a
```
Реализации по-умолчанию есть у `exponent`, `significand`, `scaleFloat`, `atan2`.

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
