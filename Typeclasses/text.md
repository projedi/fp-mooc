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

### Step 1 (Parametric & ad hoc polymorphism)

### Step 2 (Typeclasses)

### Step 3 (Similarities with Java interfaces)

### Step 4 (Contexts)

Стандартные классы типов
------------------------

### Step 1 (Eq --- "No built-in equalities")

### Step 2 (Show --- "Yet another derivable instance")

### Step 3 (Ord --- "Inheritance")

### Step 4 (Num --- "Polymorphic number literals")

Advanced typeclassing
---------------------
