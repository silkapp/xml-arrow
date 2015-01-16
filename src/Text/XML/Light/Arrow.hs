{-# LANGUAGE
    Arrows
  , TypeOperators
  #-}
{- |
List arrows for querying, creating and modifying XML trees.
-}
module Text.XML.Light.Arrow
(

  -- * Selection.

    name
  , children
  , attributes
  , key
  , value
  , text
  , kind

  -- * Filter.

  , isElem
  , isText
  , isCRef

  -- * By name.

  , elem
  , attr
  , child

  , hasAttr

  -- * Deep selection.

  , deep
  , deepWhen
  , deepWhenNot
  , deepText

  -- * Creation with only arrow components.

  , toElem
  , toAttr
  , toText

  -- * Creation with fixed components.

  , mkElem
  , mkAttr
  , mkAttrValue
  , mkText

  -- * Processing child nodes.

  , process
  , process1
  , processDeep
  , processText
  , processAttrs
  , setAttrs

  -- * Parsing / printing.

  , printXml
  , parseXml

  -- * Qualified name variants.

  , nameQ
  , keyQ
  , elemQ
  , attrQ
  , childQ
  , hasAttrQ
  , toElemQ
  , toAttrQ
  , mkElemQ
  , mkAttrQ
  , mkAttrValueQ

  ) where

import Prelude hiding (elem, id, (.))

import Control.Arrow
import Control.Arrow.ArrowList
import Control.Category
import Text.XML.Light
import qualified Data.Map as Map

nameQ :: ArrowList arr => Content `arr` QName
nameQ = arr elName . getElem

name :: ArrowList arr => Content `arr` String
name = arr qName . nameQ

children :: ArrowList arr => Content `arr` Content
children = arrL elContent . getElem

attributes :: ArrowList arr => Content `arr` Attr
attributes = arrL elAttribs . getElem

keyQ :: Arrow arr => Attr `arr` QName
keyQ = arr attrKey

key :: Arrow arr => Attr `arr` String
key = arr (qName . attrKey)

value :: Arrow arr => Attr `arr` String
value = arr attrVal

text :: ArrowList arr => Content `arr` String
text = arr cdData . getText

kind :: ArrowList arr => Content `arr` CDataKind
kind = arr cdVerbatim . getText

-- Helpers.

getElem :: ArrowList arr => Content `arr` Element
getElem = arrL (\c -> case c of Elem e -> [e]; _ -> [])

getText :: ArrowList arr => Content `arr` CData
getText = arrL (\c -> case c of Text t -> [t]; _ -> [])

----------------

isElem, isText, isCRef :: ArrowList arr => Content `arr` Content
isElem = isA (\c -> case c of Elem {} -> True; _ -> False)
isText = isA (\c -> case c of Text {} -> True; _ -> False)
isCRef = isA (\c -> case c of CRef {} -> True; _ -> False)

----------------

elemQ :: ArrowList arr => (QName -> Bool) -> Content `arr` Content
elemQ f = arrL (\c -> case c of Elem e | f (elName e) -> [c]; _ -> [])

elem :: ArrowList arr => String -> Content `arr` Content
elem n = elemQ ((==n) . qName)

attrQ :: (ArrowList arr, ArrowChoice arr) => (QName -> Bool) -> Content `arr` String
attrQ f = (isA f . keyQ `guards` value) . attributes

attr :: (ArrowList arr, ArrowChoice arr) => String -> Content `arr` String
attr n = attrQ ((==n) . qName)

childQ :: ArrowList arr => (QName -> Bool) -> Content `arr` Content
childQ f = elemQ f . children

child :: ArrowList arr => String -> Content `arr` Content
child n = childQ ((==n) . qName)

hasAttrQ :: (ArrowList arr, ArrowChoice arr) => (QName -> Bool) -> Content `arr` Content
hasAttrQ f = filterA (isA f . keyQ . attributes)

hasAttr :: (ArrowList arr, ArrowChoice arr) => String -> Content `arr` Content
hasAttr n = hasAttrQ ((==n) . qName)

----------------

deep :: (ArrowList arr, ArrowPlus arr) => (Content `arr` a) -> (Content `arr` a)
deep e = e <+> deep e . children

deepWhen :: (ArrowList arr, ArrowPlus arr, ArrowChoice arr) => Content `arr` c -> Content `arr` a -> Content `arr` a
deepWhen g e = e <+> g `guards` deepWhen g e . children

deepWhenNot :: (ArrowList arr, ArrowPlus arr, ArrowChoice arr) => Content `arr` c -> Content `arr` a -> Content `arr` a
deepWhenNot g = deepWhen (notA g)

deepText :: (ArrowPlus arr, ArrowList arr) => Content `arr` String
deepText = arr concat . list (deep text)

----------------

toElemQ :: (ArrowPlus arr, ArrowList arr) => (a `arr` QName) -> [a `arr` Attr] -> [a `arr` Content] -> a `arr` Content
toElemQ q as cs = proc i ->
  do n <- q -< i
     a <- uniques . list (concatA as) -< i
     c <- list (concatA cs) -< i
     id -< Elem (Element n a c Nothing)
  where
    uniques = arr (Map.elems . Map.fromListWith const . map (key &&& id))

toElem :: (ArrowPlus arr, ArrowList arr) => (a `arr` String) -> [a `arr` Attr] -> [a `arr` Content] -> a `arr` Content
toElem n = toElemQ (arr unqual . n)

toAttrQ :: Arrow arr => (a `arr` QName) -> (a `arr` String) -> a `arr` Attr
toAttrQ q s = proc i ->
  do n <- q -< i
     v <- s -< i
     id -< Attr n v

toAttr :: Arrow arr => (a `arr` String) -> (a `arr` String) -> a `arr` Attr
toAttr n = toAttrQ (arr unqual . n)

toText :: Arrow arr => String `arr` Content
toText = arr (\t -> Text (CData CDataText t Nothing))

----------------

mkElemQ :: (ArrowPlus arr, ArrowList arr) => QName -> [a `arr` Attr] -> [a `arr` Content] -> a `arr` Content
mkElemQ q = toElemQ (arr (const q))

mkElem :: (ArrowPlus arr, ArrowList arr) => String -> [a `arr` Attr] -> [a `arr` Content] -> a `arr` Content
mkElem n = toElem (arr (const n))

mkAttrQ :: Arrow arr => QName -> String `arr` Attr
mkAttrQ k = toAttrQ (arr (const k)) id

mkAttr :: Arrow arr => String -> String `arr` Attr
mkAttr k = toAttr (arr (const k)) id

mkAttrValueQ :: Arrow arr => QName -> String -> a `arr` Attr
mkAttrValueQ k v = mkAttrQ k . arr (const v)

mkAttrValue :: Arrow arr => String -> String -> a `arr` Attr
mkAttrValue k v = mkAttr k . arr (const v)

mkText :: Arrow arr => String -> a `arr` Content
mkText t = toText . arr (const t)

----------------

process :: (ArrowApply arr, ArrowList arr, ArrowChoice arr) => [Content] `arr` [Content] -> Content `arr` Content
process a = processor `when` isElem
  where processor = proc (Elem (Element n b c l)) ->
                    do s <- a -< c
                       id -<< Elem (Element n b s l)

process1 :: (ArrowApply arr, ArrowList arr, ArrowChoice arr) => Content `arr` Content -> Content `arr` Content
process1 a = process (list (a . unlist))

-- | If the condition holds, apply the arrow and continue processing
-- the children. Otherwise, do nothing and stop recursing.
processDeep :: (ArrowApply arr, ArrowList arr, ArrowChoice arr) => Content `arr` c -> Content `arr` Content -> Content `arr` Content
processDeep c a = (process1 (processDeep c a) . a) `when` c

processText :: ArrowList arr => String `arr` String -> Content `arr` Content
processText a = toText . a . text

processAttrs :: (ArrowPlus arr, ArrowList arr, ArrowChoice arr) => (Content `arr` Attr) -> Content `arr` Content
processAttrs attrArr = toElem name [attrArr] [children] `when` isElem

-- | When the input is an element, calculate attributes from the
-- input, and add them to the attributes already on the input element.
setAttrs :: (ArrowList arr, ArrowPlus arr, ArrowChoice arr) => (Content `arr` Attr) -> Content `arr` Content
setAttrs attrArr = processAttrs (concatA [attributes, attrArr])

----------------

printXml :: Arrow arr => Content `arr` String
printXml = arr showContent

parseXml :: ArrowList arr => String `arr` Content
parseXml = arrL parseXML
