module GUI.Elements.Button
  ( importantBtn
  , linkBtn ) where

--------------------------------------------------------------------------------
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Elements as Elems (button)
--------------------------------------------------------------------------------

-- | Кнопка, описывающая некоторое важное действие,
-- обычно приводящее к переходу на следующий логический этап.
importantBtn :: String -> UI Element
importantBtn btnText = Elems.button # set text btnText
                                    # set (attr "class") "btn-important"

-- | Кнопка-ссылка, описывающая некоторое
-- действие вторичного характера.
linkBtn :: String -> UI Element
linkBtn btnText = Elems.button # set text btnText
                               # set (attr "class") "btn-link"
