{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe

data Usuario = Usuario {
nombre :: String,
billeterainicial :: Float
} deriving(Show,Eq)


pepe = Usuario "Jose" 10
lucho = Usuario "Luciano" 2


deposito dineroAdepositar usuario =
  usuario{billeterainicial = billeterainicial usuario + dineroAdepositar}

extraccion dineroAretirar usuario =
 usuario {billeterainicial =max 0 (billeterainicial usuario - dineroAretirar) }

verificarUpdate usuario
      | (billeterainicial usuario)  * 0.2 > 10 = 10
      | otherwise = (billeterainicial usuario) * 0.2

update usuario =
     usuario { billeterainicial = billeterainicial usuario + (verificarUpdate usuario)}

cierreDeCuenta usuario =
          usuario { billeterainicial = 0}

quedaIgual usuario = usuario
