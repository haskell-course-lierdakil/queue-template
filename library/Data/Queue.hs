module Data.Queue (Queue, empty, push, pop) where

import Data.Queue.Internal

empty :: Queue a
empty = Queue [] []

-- Добавить элемент в конец очереди
push :: a -> Queue a -> Queue a
push = undefined

-- Удалить элемент из начала очереди
pop :: Queue a -> Maybe (a, Queue a)
pop q = undefined
