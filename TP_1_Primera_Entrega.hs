{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
{- Falta agregar import Test.Hspec -}

type Nombre = String
type Dinero = Float
type Evento = Usuario -> Usuario
type Transacción = Usuario -> Evento

data Usuario = Usuario {
nombre :: Nombre,
billetera :: Dinero
} deriving(Show,Eq)

nuevoNombre otroNombre usuario = usuario {nombre = otroNombre}

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
upgrade usuario = nuevoSaldo (billetera usuario + verificarUpgrade usuario) usuario

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
{-
####################################################################
#  Comentarios de las consultas #
#####################################################################

######
###   consultas con una  billetera  de 10 monedas #####
#####
1)
*Main> billetera (deposito 10 pepe)
20.0
2)
*Main> billetera (extraccion 3 pepe)
7.0
3)
*Main> billetera (extraccion 15 pepe)
0.0
4)
*Main> billetera (upgrade pepe)
12.0
5)
*Main> billetera (cierreDeCuenta pepe)
0.0
6)
*Main> billetera (quedaIgual pepe)
10.0
7)
*Main> billetera ((upgrade.deposito 1000) pepe)
1020.0
######
###   Consultas para el usuario pepe #####
#####
8)
*Main> billetera pepe
10.0
9)
*Main> billetera (cierreDeCuenta pepe)
0.0
10)
*Main> billetera ((upgrade.extraccion 2.deposito 15) pepe)
27.6
######
###   Consultas  usando las funciones   #####
###   transacción1 y transacción2       #####
#####

11)
*Main> transacción1 pepe
<function>
*Main> billetera(transacción1 pepe pepe2)
20.0
12)
*Main> transacción2 pepe
<function>
*Main> billetera(transacción2 pepe pepe2)
25.0
13)
*Main> transacción2 pepe2
<function>
*Main> billetera(transacción2 pepe2 (nuevoSaldo 50 pepe))
55.0
######
###   Consultas  usando las funciones transacción3 y   #####
###   transacción4 con una billeta de 10 monedas       #####
#####
14)
*Main> transacción3 lucho
<function>
*Main> billetera(transacción3 lucho pepe)
0.0
15)
*Main> transacción4 lucho
<function>
*Main> billetera(transacción4 lucho pepe)
34.0
######
###   Consultas  usando la funcion transacción5   #####
###   con una billetera de 10 monedas             #####
#####

16)
*Main> transacción5 pepe
<function>
*Main> billetera(transacción5 pepe pepe)
3.0
17)
*Main> transacción5 lucho
<function>
*Main> billetera(transacción5 lucho pepe)
17.0



 -}
