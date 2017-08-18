{-# OPTIONS_GHC -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 08
--
----------------------------------------------------------------------

module Party where

import Employee
import Data.Tree

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL empList fun) = 
    GL (employee:empList) (fun + (empFun employee))

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL list1 fun1) (GL list2 fun2) = GL (list1 ++ list2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun guestList1 guestList2 
    | guestList1 > guestList2 = guestList1
    | otherwise = guestList2

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------



----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel = undefined


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

maxFun :: Tree Employee -> GuestList
maxFun = undefined


----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

main :: IO ()
main = undefined
