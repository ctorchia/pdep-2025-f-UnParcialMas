module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type PowerUp = Personaje -> Personaje

data Personaje = UnPersonaje{
    nombrePersonaje :: String,
    nivelInteligencia :: Number,
    nivelVagancia :: Number,
    powerUps :: [PowerUp],
    titulos :: [String]
}deriving (Show,Eq)

mordecai = UnPersonaje {
    nombrePersonaje = "Mordecai",
    nivelInteligencia = 90,
    nivelVagancia = 60,
    powerUps = [videojuegos,taeKwonMortal "Bloqueo",picante],
    titulos = ["Extrahuevordinario","Golpe Mortal"]
}

rigby = UnPersonaje {
    nombrePersonaje = "Rigby",
    nivelInteligencia = 65,
    nivelVagancia = 101,
    powerUps = [cafeCafe 200,taeKwonMortal "Golpe"],
    titulos = []
}

-- Punto 2 A --
fiestaLosMartes :: PowerUp
fiestaLosMartes = cambiarNombreBullicio . reducirVagancia 10

reducirVagancia :: Number -> Personaje -> Personaje
reducirVagancia nivel personaje = personaje {nivelVagancia = max 0 (nivelVagancia personaje - nivel)} 

cambiarNombreBullicio :: Personaje -> Personaje
cambiarNombreBullicio personaje = personaje {nombrePersonaje = nombrePersonaje personaje ++ " Bullicio"}

-- Punto 2 B --
taeKwonMortal :: String -> Personaje -> Personaje
taeKwonMortal movimiento personaje | elem movimiento (titulos personaje) = personaje
                                   | otherwise = personaje{titulos = (movimiento ++ " Mortal") : titulos personaje}

-- Punto 2 C --
menteMax :: Personaje -> Personaje
menteMax = subirInteligencia 10 . agregarTitulo "Moriones"

agregarTitulo :: String -> Personaje -> Personaje
agregarTitulo titulo personaje = personaje{titulos= titulo : titulos personaje}

subirInteligencia :: Number -> Personaje -> Personaje
subirInteligencia nivel personaje = personaje{nivelInteligencia= nivelInteligencia personaje * (1+10/100)}

-- Punto 2 D --
videojuegos :: Personaje -> Personaje
videojuegos = aumentarVagancia 35 . subirInteligencia 5 . agregarTitulo "Maestro de los videojuegos"

aumentarVagancia :: Number -> Personaje -> Personaje
aumentarVagancia nivel personaje = personaje{nivelVagancia = max 0 (nivelVagancia personaje + nivel)}

-- Punto 2 E --
cafeCafe :: Number -> Personaje -> Personaje
cafeCafe nivel = subirInteligencia (nivel/200) . reducirVagancia 100

-- Punto 2 F --
picante :: Personaje -> Personaje
picante = id

-- Punto 3 --
puedeRealizarMision :: Mision -> Personaje -> Bool
puedeRealizarMision mision personaje = tieneAlMenosUnPowerUp personaje && mision personaje

tieneAlMenosUnPowerUp :: Personaje -> Bool
tieneAlMenosUnPowerUp = (>=1) . length . powerUps  

-- Punto 3 A --
type Mision = Personaje -> Bool
desafioExtrahuevordinario :: Mision
desafioExtrahuevordinario personaje = elem "Extrahuevordinario" (titulos personaje)

-- Punto 3 B --
darCarinio :: Mision
darCarinio personaje | nombrePersonaje personaje == "Rigby" = False 
                     | otherwise = True 

-- Punto 3 C --
beberMissisipiQueen :: Mision
beberMissisipiQueen personaje = perteneceAGrupo personaje && noEsVago personaje

grupoQuePuedeHacerMisionMissisipiQueen :: [String]
grupoQuePuedeHacerMisionMissisipiQueen = ["Mordecai","Benson","Rigby"]

perteneceAGrupo :: Personaje -> Bool
perteneceAGrupo personaje = elem (nombrePersonaje personaje) grupoQuePuedeHacerMisionMissisipiQueen

noEsVago :: Personaje -> Bool
noEsVago = (<70).nivelVagancia

-- Punto 3 D --
comerSandwichDeLaMuerte :: Personaje -> Bool
comerSandwichDeLaMuerte personaje = any (esTituloMortal) (titulos personaje)

esTituloMortal :: String -> Bool
esTituloMortal = (=="latrom").take 6.reverse

-- Punto 5 --
esGrupoRegular :: [Personaje] -> Mision -> Bool
esGrupoRegular personajes mision = unoSeLlamaPapaleta personajes || masDeTresPuedenRealizarMision mision personajes

unoSeLlamaPapaleta :: [Personaje] -> Bool
unoSeLlamaPapaleta personajes = any ((== "Papaleta") . nombrePersonaje) personajes

masDeTresPuedenRealizarMision :: Mision -> [Personaje] -> Bool
masDeTresPuedenRealizarMision mision = (>3) . length . filter (puedeRealizarMision mision)

-- Punto 6 --
versionSuprema :: Personaje -> Personaje
versionSuprema personaje = foldl (\unPersonaje unPowerUp -> unPowerUp unPersonaje) personaje (powerUps personaje)




