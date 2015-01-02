# Step 1 (Num)

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

# Step 2 (Real)

Вспомогательный класс, служащий индикатором, что тип является числом, то есть
может быть преобразован в `Rational` (рациональная дробь с числителем и знаменателем
имеющим тип `Integer`):
```
class (Num a, Ord a) => Real a where
   toRational :: a -> Rational
```

# Step 3 (Integral)

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

# Step 4 (Fractional)

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

# Step 5 (Floating)

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

# Step 6 (RealFrac)

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

# Step 7 (RealFloat)

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
