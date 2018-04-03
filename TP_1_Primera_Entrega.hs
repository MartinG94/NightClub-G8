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

verificarUsuario usuario usuarioAComparar = nombre usuario == nombre usuarioAComparar

transacción1 usuario
   | verificarUsuario usuario lucho  = cierreDeCuenta usuario
   | otherwise = quedaIgual usuario

transacción2 usuario
  | verificarUsuario usuario pepe = deposito 5 usuario
  | otherwise = quedaIgual usuario

-- ############# comentarios de las consultas ########################
{-
1)
*Main> billeterainicial (deposito 10 pepe)
20.0
2)
*Main> billeterainicial (extraccion 3 pepe)
7.0
3)
*Main> billeterainicial (extraccion 15 pepe)
0.0
4)
*Main> billeterainicial (update pepe)
12.0
5)
*Main> billeterainicial (cierreDeCuenta pepe)
0.0
6)
*Main> billeterainicial (quedaIgual pepe)
10.0
7)
*Main> billeterainicial ((update.deposito 1000) pepe)
1020.0
8)
*Main> billeterainicial pepe
10.0
9)
*Main> billeterainicial (cierreDeCuenta pepe)
0.0
10)
*Main> billeterainicial ((update.extraccion 2.deposito 15) pepe)
27.6
 -}
