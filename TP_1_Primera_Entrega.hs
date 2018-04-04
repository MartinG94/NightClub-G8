{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
{- Falta agregar Import Test.Hspect -}

type Nombre -> String
type Dinero = Float
type Evento = Usuario -> Usuario
type Transacción = Usuario -> Evento

data Usuario = Usuario {
nombre :: Nombre,
billetera :: Dinero
} deriving(Show,Eq)

nuevoSaldo otroSaldo usuario = usuario {billetera = otroSaldo}

pepe = Usuario {nombre = "Jose", billetera = 10}
pepe2 = Usuario {nombre = "Jose", billetera = 20}
lucho = Usuario {nombre = "Luciano", billetera = 2}
luxito = Usuario {nombre = "Jorge", billetera = 5}

deposito :: Dinero -> Evento
deposito dineroDepositado usuario = nuevoSaldo (billetera usuario + dineroDepositado) usuario

extraccion :: Dinero -> Evento
extraccion dineroARetirar usuario = nuevoSaldo ( max 0 (billetera usuario - dineroARetirar) ) usuario

upgrade :: Evento
upgrade usuario = nuevoSaldo (billetera usuario + verificarUgrade usuario) usuario

verificarUpgrade usuario
      | billetera usuario * 0.2 > 10 = 10
      | otherwise = billetera usuario * 0.2

cierreDeCuenta :: Evento
cierreDeCuenta usuario = nuevoSaldo 0 usuario

quedaIgual :: Evento
quedaIgual usuario = usuario

verificarUsuario usuarioAComparar usuario = nombre usuarioAComparar == nombre usuario

transacción1 :: Transacción
transacción1 usuario
      | verificarUsuario lucho usuario = cierreDeCuenta
      | otherwise = quedaIgual

transacción2 :: Transacción
transacción2 usuario
      | verificarUsuario pepe usuario = deposito 5
      | otherwise = quedaIgual

tocoYMeVoy :: Evento
tocoYMeVoy usuario = (cierreDeCuenta . upgrade . deposito 15) usuario

ahorranteErrante :: Evento
ahorranteErrante usuario  = (deposito 10 . upgrade . deposito 8 . extraccion 1 . deposito 2 . deposito 1) usuario

transacción3 :: Transacción
transacción3 usuario
      | verificarUsuario lucho usuario = tocoYMeVoy
      | otherwise = quedaIgual

transacción4 :: Transacción
transacción4 usuario
      |verificarUsuario lucho usuario = ahorranteErrante
      | otherwise = quedaIgual

transacción5 :: Transacción
transacción5 usuario
      | verificarUsuario lucho usuario = deposito 7
      | verificarUsuario pepe usuario = extraccion 7
      | otherwise = quedaIgual

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
