-- -- -- -- -- -- -- -- -- -- 2. Números enteros -- -- -- -- -- -- -- -- -- --

------------------ 2.1.a ------------------
sucesor :: Int -> Int
sucesor numero = numero + 1
 

------------------ 2.1.b ------------------
sumar :: Int -> Int -> Int
sumar n m = n + m

------------------ 2.1.c ------------------
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto dividendo divisor = (div dividendo divisor, mod dividendo divisor)

-- PRECONDICIONES: el dividendo deber ser distinto a cero.

------------------ 2.1.d ------------------
maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if n > m
                   then n 
                   else m


------------------ 2.2 ------------------
-- sumar (maxDelPar(divisionYResto 0 1)) (sucesor 9)
-- sucesor (maxDelPar (divisionYResto (sumar 33 33) 7))
-- maxDelPar ((sumar 5 5), (maxDelPar (divisionYResto 0 (sucesor 3))))
-- sumar (maxDelPar (divisionYResto 5 (sucesor 0))) 5









-- -- -- -- -- -- -- -- -- -- 3. Tipos Enumerativos -- -- -- -- -- -- -- -- -- --

data Dir = Norte | Oeste | Sur | Este
    deriving Show  

------------------ 3.1.a ------------------
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Oeste = Este
opuesto Este = Oeste


------------------ 3.1.b ------------------
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True 
iguales Sur Sur     = True
iguales Este Este   = True 
iguales Oeste Oeste = True
iguales _ _         = False

------------------ 3.1.c ------------------
siguiente ::Dir -> Dir
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Oeste
siguiente _     = error "No existe siguiente de Oeste."

{- PRECONDICIONES: No puede ingresar la dirección Oeste.
   Es una función parcial porque no funciona con todas...
-}





data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show
    
primerDia :: DiaDeSemana
primerDia = Lunes

ultimoDia :: DiaDeSemana
ultimoDia = Domingo

------------------ 3.2.a ---------------------
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDia, ultimoDia)

------------------ 3.2.b ---------------------
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM _         = False 

------------------ 3.2.c ---------------------

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues _          Domingo     = False
vieneDespues Domingo    Sabado      = True
vieneDespues _          Sabado      = False
vieneDespues Jueves     Sabado      = False
vieneDespues Jueves     Viernes     = False
vieneDespues Miercoles  Sabado      = False
vieneDespues Miercoles  Viernes     = False
vieneDespues Miercoles  Jueves      = False
vieneDespues Martes     _           = False
vieneDespues _          Lunes       = True
vieneDespues _          _           = True


------------------ 3.2.d ---------------------
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _     = True      -- Porque no funciona con primerDia o ultimoDia?


------------------ 3.3.a ------------------
negar :: Bool -> Bool
negar True = False
negar _    = True

------------------ 3.3.b ------------------
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _        = True

------------------ 3.3.c ------------------
yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _ _ = False

------------------ 3.3.d ------------------
oBien :: Bool -> Bool -> Bool
oBien False False = False
oBien _ _ = True


-- -- -- -- -- -- -- -- -- -- 4. Registros -- -- -- -- -- -- -- -- -- --

------------------ 4.1 ------------------
data Persona = P String Int
           --    Nombre Edad
    deriving Show 

sergio = P "Sergio" 33
jose = P "Jose" 69

nombre :: Persona -> String     
nombre (P nom _) = nom

edad :: Persona -> Int
edad (P _ edad) = edad

crecer :: Persona -> Int
crecer (P _ edad) = sucesor edad

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nombre persona = P nombre (edad persona)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra persona1 persona2 = if (edad persona1) > (edad persona2)
                                     then True
                                     else False

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor persona1 persona2 = if esMayorQueLaOtra persona1 persona2
                                then persona1
                                else persona2


------------------ 4.2 ------------------

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show

data Entrenador = E String Pokemon Pokemon
    deriving Show
    
data Pokemon = Poke TipoDePokemon Int
    deriving Show

charmander = Poke Fuego 90
bulbasaur = Poke Planta 65
squirtle = Poke Agua 85
magickarp = Poke Agua 30

ash = E "Ash" charmander magickarp
brock = E "Brock" squirtle bulbasaur

tipoDePoke :: Pokemon -> TipoDePokemon     -- Observadora de TipoDePokemon
tipoDePoke (Poke tipo _) = tipo

energiaDePoke :: Pokemon -> Int     -- Observadora de Energia del Pokemon
energiaDePoke (Poke _ energia) = energia




superaA :: Pokemon -> Pokemon -> Bool
superaA poke1 poke2 = tipo_SuperaA_ (tipoDePoke poke1) (tipoDePoke poke2)

tipo_SuperaA_ :: TipoDePokemon -> TipoDePokemon -> Bool
tipo_SuperaA_ Agua Fuego = True
tipo_SuperaA_ Fuego Planta = True
tipo_SuperaA_ Planta Agua = True
tipo_SuperaA_ _ _= False


cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipo entrenador = energiaDePoke (pokemonDeTipo_De_ tipo entrenador)
-- OBSERVACIÓN: Si el entrenador tiene dos Pokemones del tipo dado, la función devolverá el primero de ellos.


pokemonDeTipo_De_  :: TipoDePokemon -> Entrenador -> Pokemon
pokemonDeTipo_De_ tipo (E _ poke1 poke2) = if sonDelMismoTipo tipo (tipoDePoke poke1)
                                                    then poke1
                                                    else if (sonDelMismoTipo tipo (tipoDePoke poke2))
                                                        then poke2
                                                        else error "El entrenador no posse un Pokemon de ese tipo."

sonDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonDelMismoTipo Agua Agua     = True
sonDelMismoTipo Fuego Fuego   = True
sonDelMismoTipo Planta Planta = True
sonDelMismoTipo _ _           = False

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon ((E _ poke1 poke2), (E _ poke3 poke4)) = poke1:poke2:poke3:poke4:[] 





-- -- -- -- -- -- -- -- -- -- 5. Funciones Polimórficas -- -- -- -- -- -- -- -- -- --

loMismo :: a -> a
loMismo a = a

siempreSiete :: a -> Int
siempreSiete a = 7

swap :: (a,b) -> (b, a)
swap (a,b) = (b,a)

{- Estas son funciones polimórficas porque pueden recibir diferentes Tipos de Datos 
y funcionan en todos los casos. -}






-- -- -- -- -- -- -- -- -- 6. Pattern Matching Sobre Listas -- -- -- -- -- -- -- -- --


estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _  = False

elPrimero :: [a] -> a
elPrimero (x:_) = x

sinElPrimero :: [a] -> [a]
sinElPrimero (_:xs) = xs

splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x, xs)


