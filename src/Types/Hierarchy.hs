{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds        #-}

module Types.Hierarchy
  ( RecStage(..), Stage(..), Start(..), Auth(..), Reg(..), Recovery(..)
  , Callable(..), Checkable(..), Manageable(..), Form(..), NonfunForm(..) )
  where

--------------------------------------------------------------------------------
-- Описание процесса передачи информации от клиента к серверу.
-- GUI форма -> Обработка данных -> Пре-проверка данных -> Передача серверу
--------------------------------------------------------------------------------

import Graphics.UI.Threepenny.Core (UI, Element)
import Network.Socket              (Socket)
import Types.General               (FlagAssociated(..))
import Types.Results               (Result(..))


data RecStage =
  SendingEmail
 |SendingKey
 |ChangePassword

data Start       = Start
data Auth        = Auth
data Reg         = Reg
newtype Recovery = Recovery RecStage

class Stage t where
instance Stage Start
instance Stage Auth
instance Stage Recovery

type Value            = String
type Hierarchical s r = (Stage s, FlagAssociated r, Result r)

class Hierarchical s r => Callable s r | s -> r where
 -- | Передача данных серверу с возвращением результата.
 call :: s -> Socket -> [Value] -> IO r

class Hierarchical s r => Checkable s r | s -> r where
 -- | Может вернуть результат если пользователь передал заведомо
 -- невозможные данные, иначе же работа делегируется уровню Callable.
 check :: s -> [Value] -> Maybe r

class (Callable s r, Checkable s r) => Manageable s r | s -> r where
 -- | Делегирует работу уровням Checkable и Callable.
 -- В зависимости от результата работы этих уровней оповещает пользователей.
 handle :: s -> Socket -> UI ()

class (Manageable s r) => Form s r | s -> r where
  -- | Представляет элемент формы, выводящейся пользователю.
  -- Имеет обработчик событий, который при определенных действиях пользователя
  -- делегирует работу уровню Manageable.
  form :: s -> Socket -> UI Element

class NonfunForm s where
  -- | Форма, не описывающая какое-либо действие кроме перехода на
  -- следующий этап.
  nonfunForm :: s -> Socket -> UI Element
