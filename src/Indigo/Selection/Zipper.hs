{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Selection.Zipper
  ( SelectionZipper (..)
  , left
  , right
  )
where

import Data.Sequence (Seq (..), (<|), (|>))

data SelectionZipper a = SelectionZipper
  { lefts :: Seq a
  , focus :: a
  , rights :: Seq a
  }

left :: SelectionZipper a -> SelectionZipper a
left zipper =
  case (zipper.lefts, zipper.rights) of
    (Empty, Empty) ->
      zipper

    (lefts :|> focus, _) ->
      SelectionZipper
        { lefts
        , focus
        , rights = zipper.focus <| zipper.rights
        }

    (Empty, lefts :|> focus) ->
      SelectionZipper
        { lefts
        , focus
        , rights = Empty
        }

right :: SelectionZipper a -> SelectionZipper a
right zipper =
  case (zipper.lefts, zipper.rights) of
    (Empty, Empty) ->
      zipper

    (_, focus :<| rights) ->
      SelectionZipper
        { lefts = zipper.lefts |> zipper.focus
        , focus
        , rights
        }

    (focus :<| rights, Empty) ->
      SelectionZipper
        { lefts = Empty
        , focus
        , rights
        }
