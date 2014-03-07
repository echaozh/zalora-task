> module Web.ZaloraTask.View where

View part of MVC. Not very pretty, but it got picutres on all pages.

  <!--

> import Control.Monad

> import Data.Char
> import Data.Monoid
> import Data.Text.Lazy (Text, pack)

> import Database.Persist.Sql (Entity(..), unKey)

  -->

I use [blaze-html](http://jaspervdj.be/blaze/‎) for HTML building and rendering,
therefore the imports:

> import Text.Blaze.Html5 ((!), toHtml, toValue)
> import qualified Text.Blaze.Html5 as H
> import qualified Text.Blaze.Html5.Attributes as A
> import Text.Blaze.Html.Renderer.Text

  <!--

> import Web.PathPieces

> import Web.ZaloraTask.Model

  -->

To make the HTML structure obvious, I deviate a bit from my habbit to put all
operators at the beginning of the line to mark a line continuation. The HTML
building specific conventions are:

1. all child nodes: tags, attributes and texts, always begin their own lines,
and are indented more than the tag;
1. attributes always follow `!`, and texts always follow `$`, while tags _never_
follow `$`;
1. `forM_`, `when` and `unless`, as embedded flow constructs, are indented more
than the parent tag, with their children indented even more;

Hopefully, the convention makes the code tidy and as easy to understand as
[Jade](http://jade-lang.com) templates

> showPage :: Text -> ShoeGeneric backend -> Text
> showPage shoeId shoe = renderHtml . H.docTypeHtml $ do
>   let title = toHtml $ "Shoe #" <> shoeId
>   H.head $ do
>     H.title
>       $ title
>     H.style
>       ! A.type_ "text/css"
>       $ "label {margin-right: 20px}"
>   H.body $ do
>     H.h1
>       $ title
>     H.p $ do
>       H.img
>         ! A.id "photo"
>         ! A.src (toValue ("/" <> shoePhoto shoe <> ".jpg"))
>       forM_ [("description", shoeDescription), ("color", shoeColor),
>              ("size", pack . show . shoeSize)] $ \(s, f) ->
>         H.p $ do
>           H.label
>             ! A.for (toValue s) $
>             H.strong
>               $ toHtml $ toUpper (head s) : map toLower (tail s) <> ":"
>           H.span
>             ! A.id (toValue s)
>             $ toHtml $ f shoe

There is the paged listing page, and the unpaged one. The shoe listings should
be the same, while there is no navigation bar in the page without paging.
Therefore, we use a layout and 2 partials to reuse code.

> listLayout :: H.Html -> H.Html -> Text
> listLayout nav shoes = renderHtml . H.docTypeHtml $ do
>   let title = "Shoe Listing"
>   H.head $ do
>     H.title
>       $ title
>     H.style
>       ! A.type_ "text/css"
>       $ "#prev {margin-right: 10px;} #next {margin-left: 10px;}"
>     H.style
>       ! A.type_ "text/css"
>       $ (".shoeBox {border: 1px solid; padding: 10px; display: table-cell; "
>          <> "margin: 10px; float: left;}")
>     H.style
>       ! A.type_ "text/css"
>       $ ".photo {height: 200px; width: 200px;}"
>     H.style
>       ! A.type_ "text/css"
>       $ ".shoe {text-align: center;}"
>   H.body $ do
>     H.h1
>       $ title
>     nav
>     shoes
>
> navPartial :: Int -> Int -> H.Html
> navPartial page pages = H.nav $ do
>   when (page > 1) $
>     H.a
>       ! A.id "prev"
>       ! A.href (toValue $ "/shoes?p=" <> show (page - 1))
>       $ "Previous Page"
>   when (page < pages) $
>     H.a
>       ! A.id "next"
>       ! A.href (toValue $ "/shoes?p=" <> show (page + 1))
>       $ "Next Page"
>
> listPartial :: [Entity (ShoeGeneric backend)] -> H.Html
> listPartial shoes =
>   H.div $
>     forM_ shoes $ \(Entity key shoe) ->
>       let appendKey = (<> toPathPiece (unKey key))
>       in
>         H.a ! A.class_ "link"
>           ! A.href (toValue $ appendKey "/shoes/") $
>             H.figure
>               ! A.class_ "shoeBox" $ do
>               H.img
>                 ! A.class_ "photo"
>                 ! A.src (toValue ("/" <> shoePhoto shoe <> ".jpg"))
>               H.figcaption
>                 ! A.class_ "shoe"
>                 $ toHtml $ appendKey "Shoe #"
