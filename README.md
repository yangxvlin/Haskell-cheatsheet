# Haskell-cheatsheet
my Haskell cheatsheet repository

## Prelude funcitons
- http://www.cse.chalmers.se/edu/year/2018/course/TDA555/tourofprelude.html

## conditions
### if-then-else
- ``` haskell
    -- Definition A
    iota n = 
        if n == 0 
        then 
            [] 
        else 
            iota (n-1) ++ [n]
    ```
### Guards
- ``` haskell
    -- Definition B
    iota n
        | n == 0 = []
        | n > 0 = iota (n-1) ++ [n]
        | otherwise = ...
    ```
### Parametric polymorphism
- ``` haskell
    len :: [t] -> Int
    len [] = 0
    len (_:xs) = 1 + len xs
    ```
### case _ of
- ``` haskell
    data Expr
        = Number Int
        | Variable String
        | Binop Binopr Expr Expr
        | Unop Unopr Expr
    data Binopr = Plus | Minus | Times | Divide
    data Unopr = Negate
    ```
- ``` haskell
    is_static :: Expr -> Bool
    is_static expr =
        case expr of
            Number _ -> True
            Variable _ -> False
            Unop _ expr1 -> is_static expr1
            Binop _ expr1 expr2 -> is_static expr1 && is_static expr2
    ```
- A case expression must have 
    - **at least one alternative and each alternative must have at least one body**. 
    - Each body must have the same type, and the type of the whole expression is that type.
## let & where
- ``` haskell
    let name1 = expr1
        name2 = expr2
    in mainexpr
    ```
- ``` haskell
    mainexpr
    where
        name1 = expr1
        name2 = expr2
    ```
## Haskell types
- ``` haskell
    data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | Jack | Queen | King | Ace
    data Suit = Club | Diamond | Heart | Spade
        deriving (Show, Eq, Ord)
    data Card = Card Suit Rank

    instance Show Rank where show = showrank
    showrank :: Rank -> String
    showrank R2 = "R2"
    showrank R3 = "R3"
    -- 或者像上面一样derive
    ```
### type polymorphism
- ``` haskell
    data Maybe t = Nothing | Just t
    ```
## Tree
- ``` haskell
    data Tree a = Leaf | Node (Tree a) a (Tree a)
    ```
- ``` haskell
    countnodes :: Tree -> Int
    countnodes Leaf = 0
    countnodes (Node l _  r) = 1 + (countnodes l) + (countnodes r)
    ```
- ``` haskell
    data Tree = Leaf | Node String Int Tree Tree

    search_bst :: Tree -> String -> Maybe Int
    search_bst Leaf _ = Nothing
    search_bst (Node k v l r) sk
        | sk == k = Just v
        | sk < k = search_bst l sk
        | otherwise = search_bst r sk
    ```
### Polymorphic Tree
- ``` haskell
    data Tree k v = Leaf | Node k v (Tree k v) (Tree k v)

    type IntTree = Tree Int String
    ```
- ``` haskell
    countnodes :: Tree k v -> Int

    search_bst :: Ord k => Tree k v -> k -> Maybe v
    ```

