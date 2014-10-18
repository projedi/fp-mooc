---
vim: ft=markdown
---

# What I know about typeclasses

* Что такое параметрический и ad-hoc полиморфизм:
   - Parametric: Works the same way for every single type
   - Ad-hoc: For every type there is a special implementation
* Как ad-hoc работает в Haskell:
   - Typeclasses as a number of methods
   - Class in a more mathematical sense
* Чем похоже, чем отличается от интерфейсов в Java
* Стандартные классы типов:
   - Eq (not everything has decidable equality)
   - Show (yet another derivable instance)
   - Ord (inheritance)
   - Num (polymorphic number literals)
* Тонкости:
   - Deriving + standalone deriving
   - Multi-parameter + fundeps
   - Orphan instances (and how to segfault with 'em)
   - Overlapping + undecidable instances
* Implementation of typeclasses. How do they look in Core:
   - Dictionary passing
