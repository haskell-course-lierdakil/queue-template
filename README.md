# queue-template

Шаблон для реализации Queue, с тестами корректности и производительности.

Все функции, которые необходимо реализовать, находятся в `library/Data/Queue.hs`

В `library/Data/Queue/Internal.hs` определён сам тип `Queue`.

Для запуска тестов корректности: `cabal v2-test` или `stack test`

Для запуска тестов производительности: `cabal v2-bench` или `stack bench`

От тестов производительности ожидается, что только время первого `pop` растёт от размера очереди, прочие операции O(1).
