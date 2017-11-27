# Frames - GUI elements managing system.

It allows us to create data-independent, multi-language and type-safe interfaces, based on WebKit.  
It *separates structure from data* and let us think about this division in a more Haskell way.  

Frame is just a set of somehow related elements.  
Current GUI state could be described by the set of frames.  
Thus, we get more control over the GUI state.  

## Use case example

### Data description

Let's say we're going to create the frame contains a header and a button.  
And some part of the header must be **bolded**.

``` haskell
{-# LANGUAGE DeriveDataTypeable #-}

data SomeFrameData = SomeFrameData
  { hdrSome :: (Text, Text)
  , btnSome ::  Text
  } deriving (Typeable)
```

We need to make ```SomeFrameData``` be both an instance of a ```Default```
and a ```LocalizedClass``` classes to get it localized.  

``` haskell
instance Default SomeFrameData where
  def = SomeFrameData ("Usual intro before", "the most important part.") "Click me"

instance LocalizedClass SomeFrameData where
  translate ENG _ = def
  translate RUS _ = SomeFrameData ("Стандартное вступление перед", "важным сообщением.") "Нажми меня"
```

### Binding with the structure

The easiest way to describe structure is to create a ```SomeFrameData -> [Token]``` function.  

``` haskell
{-# LANGUAGE RecordWildCards #-}

toSomeHtml :: SomeFrameData -> [Token]
toSomeHtml SomeFrameData{..} = TagOpen "div" []
                             :   TagOpen "div" []
                             :     ContentText (fst hdrSome)
                             :     TagOpen "b" [] : ContentText (snd hdrSome) : TagClose "b"
                             :   TagClose "div"
                             :   TagOpen "button" []
                             :     ContentText btnSome
                             :   TagClose "button"
                             : TagClose "div" : []
```

To make this structure represent the frame we need to make ```SomeFrameData```
be the instance of a ```FrameClass``` class, which definition is the following:    

``` haskell
type FrameElements' a = FrameElements a Element
newtype ReleaseFunction = ReleaseFunction { runReleaser :: IO () }

class (Typeable frameData) => FrameClass frameData where
  data FrameElements frameData :: * -> *
  updateFrame :: (DocumentClass doc) => doc -> (ReleaseFunction, FrameElements' frameData)
                                     -> frameData
                                     -> IO ()
  initFrame   :: (DocumentClass doc) => doc
                                     -> frameData
                                     -> IO (ReleaseFunction, FrameElements' frameData)
```

```FrameElements``` represents the set of elements you're going to interact with.  

``` haskell
instance FrameClass SomeFrameData where
  data FrameElements SomeFrameData el = SomeFrameElements { elHeader, elButton }
  initFrame doc someData = do
    (Just body) <- Doc.getBody doc
    -- The 'tokensToNodes' function converts HTML-tokens into the forest of WebKit elements.
    -- So the only thing we must do is to pattern-match the elements tree.
    (roots, elForest) <- tokensToNodes doc (toSomeHtml someData)
    let (Node _ [Node elHeader _, Node elButton _]) = head elForest
    -- And connect the roots with the body (or any other part of the DOM).
    appendChildren body roots
    return (defaultFunRelease body roots, SomeFrameElements{..})
```

But to make our interface translatable we must define the ```updateFrame``` function.  

``` haskell
  -- For such a tiny frame it's the easiest implementation, but we don't take into account binded events.
  updateFrame doc (funRelease, _) newFrameData = runReleaser funRelease >> initFrame doc newFrameData  
```

### Interaction with the ```AppM``` monad

Once you've done it, you may use some useful functions defined in ```Types.General``` (see it to know more).  
Let on click on the button the interface will be translated to Russian.   
A code will look like that:  

``` haskell
flip runReaderT appRef $
  withFrame (def :: SomeFrameData) $ \(SomeFrameElements{..}, _, _) -> liftIO $
    onClick btnSome $ runReaderT (translateInterface RUS) appRef
```
