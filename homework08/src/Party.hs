{-# OPTIONS_GHC -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 08
----------------------------------------------------------------------

module Party where

import Employee
import Data.Tree

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL empList fun) =
    GL (employee:empList) (fun + empFun employee)

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
-- draw employees (testing purposes only)
drawEmployees :: Tree Employee -> IO()
drawEmployees = putStr . drawTree . fmap empName

foldTree' :: (a -> [b] -> b) -> b -> Tree a -> b
foldTree' f initialValue (Node x treeList) =
    --apply function to
    --  node value as first par
    --  a list of something as second param
    --  returns a something
    f x (map (foldTree' f initialValue) treeList)
----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss guestListsPaired =
    let
        -- this is the original list with (sub)bosses
        originalWithBosses = map fst guestListsPaired

        --this is the original without bosses or boss
        originalWithoutBosses = map snd guestListsPaired

        -- Add the boss to the original list without (sub)bosses
        partialWithBosses = [] --map (glCons boss) originalWithBosses

        -- Add the boss to the original list without (sub)bosses
        partialWithoutBosses = map (glCons boss) originalWithoutBosses

        -- the list whithout the boss is the concatenation of the two original
        -- lists
        listWithoutBoss = originalWithBosses ++ originalWithoutBosses

        -- the same whith the list with the boss applied
        listWithBoss = partialWithBosses ++ partialWithoutBosses
    in
        --returns the best two (with and without the boss
        (best listWithBoss, best listWithoutBoss)
    where
        best :: [GuestList] -> GuestList
        best [] = mempty
        best guestsList = maximum guestsList

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun .foldTree' nextLevel mempty

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

main :: IO ()
main = undefined
