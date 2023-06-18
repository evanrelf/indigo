{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Selection.Zipper
  ( SelectionZipper (..)

    -- * Query
  , isBegin
  , isEnd

    -- * Move
  , left
  , right
  , begin
  , end
  )
where

import Data.Sequence (Seq (..), (<|), (|>))

import qualified Data.Sequence as Seq

data SelectionZipper a = SelectionZipper
  { lefts :: Seq a
  , focus :: a
  , rights :: Seq a
  }

isBegin :: SelectionZipper a -> Bool
isBegin zipper = Seq.null zipper.lefts

isEnd :: SelectionZipper a -> Bool
isEnd zipper = Seq.null zipper.rights

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

begin :: SelectionZipper a -> SelectionZipper a
begin zipper =
  case (zipper.lefts, zipper.rights) of
    (Empty, _) ->
      zipper

    (focus :<| rights, _) ->
      SelectionZipper
        { lefts = Empty
        , focus
        , rights = rights <> one zipper.focus <> zipper.rights
        }

end :: SelectionZipper a -> SelectionZipper a
end zipper =
  case (zipper.lefts, zipper.rights) of
    (_, Empty) ->
      zipper

    (_, lefts :|> focus) ->
      SelectionZipper
        { lefts = zipper.lefts <> one zipper.focus <> lefts
        , focus
        , rights = Empty
        }
