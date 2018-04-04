{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe

type Dinero = Float
type Evento = Usuario -> Usuario

data Usuario = Usuario {
nombre :: String,
billetera :: Dinero
} deriving(Show,Eq)

nuevoSaldo otroSaldo usuario = usuario {billetera = otroSaldo}

pepe = Usuario {nombre = "Jose", billetera = 10}
lucho = Usuario {nombre = "Luciano", billetera = 2}

deposito :: Dinero -> Evento
deposito dineroDepositado usuario = nuevoSaldo (billetera usuario + dineroDepositado) usuario

extraccion :: Dinero -> Evento
extraccion dineroARetirar usuario = nuevoSaldo ( max 0 (billetera usuario - dineroARetirar) ) usuario

update :: Evento
update usuario = nuevoSaldo (billetera usuario + verificarUpdate usuario) usuario

verificarUpdate usuario
      | billetera usuario * 0.2 > 10 = 10
      | otherwise = billetera usuario * 0.2

cierreDeCuenta :: Evento
cierreDeCuenta usuario = nuevoSaldo 0 usuario

quedaIgual :: Evento
quedaIgual usuario = usuario

verificarUsuario usuario usuarioAComparar = nombre usuario == nombre usuarioAComparar

transacción1 usuario
   | verificarUsuario usuario lucho  = cierreDeCuenta usuario
   | otherwise = quedaIgual usuario

transacción2 usuario
  | verificarUsuario usuario pepe = deposito 5 usuario
  | otherwise = quedaIgual usuario

transacción3 usuario
 | verificarUsuario usuario lucho = tocoYMevoy usuario
 | otherwise = quedaIgual usuario

transacción4 usuario
 |verificarUsuario usuario lucho = ahorranteErrante usuario
 | otherwise = quedaIgual usuario


transacción5 usuario
  | verificarUsuario usuario lucho = deposito 7 usuario
  | verificarUsuario usuario pepe = extraccion 7 usuario
  | otherwise = quedaIgual usuario

tocoYMevoy usuario = (cierreDeCuenta . update . deposito 15) usuario
ahorranteErrante usuario  = (deposito 10 . update . deposito 8 . extraccion 1 . deposito 2 . deposito 1) usuario

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
11)
*Main> transacción1 pepe
Usuario {nombre = "Jose", billeterainicial = 10.0}
12)
*Main> transacción2 pepe
Usuario {nombre = "Jose", billeterainicial = 15.0}
*Main> transacción5 pepe
Usuario {nombre = "Jose", billeterainicial = 3.0}
*Main> transacción5 lucho
Usuario {nombre = "Luciano", billeterainicial = 9.0}


 -}
