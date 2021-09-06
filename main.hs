--Trabajo Final Haskell Marcos García Martín  20/7/2021
import Data.List
import System.IO
import Data.Char
import Data.String
import System.Environment
import System.IO
import GHC.Float



{----------------------------------------------------------------------------------------------------------------
-- PROGRAMA PRINCIPAL

"Programa Trabajo Haskell "
" El programa encuentra" 
"1. La ruta ortodrómica que va de la ciudad inicial a la final, tanto la distancia como el rumbo inicial."
"2. La ruta loxodrómica que va de la ciudad inicial a la final, tanto la distancia como el rumbo."
"3. Que interpole tantos puntos como figuren en el último parámetro del fichero en la ortodrómica y las distancias y rumbos loxodrómicos de cada uno de ellos al siguiente (incluyendo en la lista el punto inicial y el final)."
 "Todos los resultados deberá escribirlos en pantalla y dejarlos en un fichero llamado resultados.txt."
----------------------------------------------------------------------------------------------------------------}

main :: IO ()
main = do 
 l <- getArgs -- Se entroduce el fichero como argumento 
 putStrLn "Programa Trabajo Haskell "
 putStrLn " El programa encuentra" 
 putStrLn "1. La ruta ortodrómica que va de la ciudad inicial a la final, tanto la distancia como el rumbo inicial."
 putStrLn "2. La ruta loxodrómica que va de la ciudad inicial a la final, tanto la distancia como el rumbo."
 putStrLn "3. Que interpole tantos puntos como figuren en el último parámetro del fichero en la ortodrómica y las distancias y rumbos loxodrómicos de cada uno de ellos al siguiente (incluyendo en la lista el punto inicial y el final)."
 putStrLn "Todos los resultados deberá escribirlos en pantalla y dejarlos en un fichero llamado resultados.txt."
 putStrLn "------------------------------------------------------------------------------------------------------"
 putStrLn "-------------------------------------------------------------------------------------------------------"
 let fentrada = head(l) -- Se escoge el primer valor de la lista 
 fichero<- readFile fentrada -- leo el contenido del fichero
 let lineaFichero = lines fichero --Guardamos las diferentes lineas del fichero en una lista , el fichero siempre debe estar en el mismo orden (igual al scs.txt dado como ejemplo) para que pueda ser utilizado correctamente
 let ciudad1 = lineaFichero!!0
 let ciudad2 = lineaFichero!!3
 putStrLn ("Ruta " ++ ciudad1 ++ "-" ++ ciudad2)--Se Imprime las ciudades de la ruta por pantalla
 let (x,y)= (lineaFichero!!1,lineaFichero!!2)-- Se guardan  las coordenadas de origen en (x,y)
 let (z,t)= (lineaFichero!!4,lineaFichero!!5)--Se guardan  las coordenadas de destino en (z,t)
 let k= lineaFichero!!6 -- Se guardan en K la cantidad de puntos intemedios 
 let string1 = "Ruta " ++ ciudad1 ++ "-" ++ ciudad2 ++ "\n" --Se guarda un string con la informacion de la ruta
 let ruta_ort = distancia_ort_f (read x,read y) ( read z, read t) -- Se calcula la distancia ortodromica  (informacion funcion mas adelante)
 let ruta_ort_f =  show ruta_ort -- utilizamos show para pasar el ruta_ort a un String
 putStrLn ("Ruta Ortodromica " ++ ruta_ort_f)
 let string2 = "Ruta Ortodromica " ++ ruta_ort_f ++ "\n" --Se guarda ruta_ort en  un string 
 let rumbo_orti = rumbo_ort_f1 (read x,read y) (read z,read t) -- Se carlcula el rumbo ortodromico de ida leeyendo con read para pasar de String a Double
 let rumbo_orti_f =  show rumbo_orti -- utilizamos show para pasar el rumbo_ort a un String
 putStrLn ("Rumbo  " ++ rumbo_orti_f)
 let string3 = ("Rumbo  " ++ rumbo_orti_f ++ "\n")
-- A partir de aqui se hara lo mismo con los diferentes datos que iremos obteniendo   se leeran con read y se calcularan con la funcion correspundiente para obtener el dato necesario, se guardaran esos datos en un string mediante un show, se guardaran en un string que se usara mas adeante y se monstraran por pantalla
 let rumbo_ortv = rumbo_ort_f1 (read z,read t) (read x,read y) --Se carlcula el rumbo ortodromico de vuelta
 let rumbo_ortv_f =  show rumbo_ortv
 putStrLn ("Rumbo Vuelta " ++ rumbo_ortv_f)
 let string4 = ("Rumbo Vuelta " ++ rumbo_ortv_f ++ "\n" )
 let ruta_lox = ruta_lox_2(read x,read y) (read z,read t) --Se calcula la distancia de la ruta loxodromica 
 let ruta_lox_f =  show ruta_lox
 putStrLn ("Ruta  Loxodromica " ++ ruta_lox_f)
 let string5 = ("Ruta  Loxodromica " ++ ruta_lox_f ++ "\n")
 let rumbo_lox_r = rumbo_lox_1 (read x,read y) (read z,read t) --Se calcula  el rumbo loxodromico de ida
 let rumbo_lox_r_f =  show rumbo_lox_r
 putStrLn ("Rumbo   " ++ rumbo_lox_r_f)
 let string6 =("Rumbo   " ++ rumbo_lox_r_f ++"\n")
 let rumbo_lox = rumbo_lox_2 (read x,read y) (read z,read t)--Se calcula  el rumbo loxodromico de regreso
 let rumbo_lox_f =  show rumbo_lox
 putStrLn ("Rumbo  Regreso " ++ rumbo_lox_f)
 let string7 = ("Rumbo  Regreso " ++ rumbo_lox_f ++ "\n" )
 let diferencia = ruta_lox - ruta_ort  -- Se calcula la diferencia entre la distancia de la ruta loxdromica y la ruta ortodromica
 let diferencia_f = show diferencia
 putStrLn ("Diferencia " ++ diferencia_f )
 let stringM =("Diferencia " ++ diferencia_f ++ "\n" )
 putStrLn "Interpolando puntos en la ortodromica:"
 let ls = lista_puntos_final (read k) (read x,read y) (read z,read t) -- Se cacula la lista de puntos intermedios
 let lsf= show ls
 putStrLn lsf 
 putStrLn "Poliloxidromica:"
 let distancias = distancias_pol (lista_rad2grad ls)  -- Se cacula la distancia entres los  puntos intermedios ya calculados
 let distancias_f = show distancias
 putStrLn ("Distancias:   " ++ distancias_f )
 let string8 = ("Distancias:   " ++ distancias_f ++ "\n" )
 let rumbos = rumbos_pol (lista_rad2grad ls) -- Se calculan los rumbos  entres los  puntos intermedios ya calculados
 let rumbos_f = show rumbos
 putStrLn ("Rumbos:   " ++ rumbos_f )
 let string9 =  ("Rumbos:   " ++ rumbos_f ++ "\n")
 let diferencia = diferencia_pol_ort (read k) (read x,read y) (read z,read t) -- Diferencia entre la Poliloxodromica y la Ortodromica
 let diferencia_f = show diferencia
 putStrLn ("Diferencia:   " ++ diferencia_f )
 let string10 = ("Diferencia:   " ++ diferencia_f ++ "\n")
 writeFile "solución.txt"  ( string1 ++ string2 ++ string3 ++ string4 ++ string5 ++ string6 ++ string7 ++ stringM ++ "Interpolando 3 puntos en la ortodromica: \n" ++ lsf ++ "\n" ++ "Poliloxidromica: \n" ++ string8 ++ string9 ++ string10  )-- se escribe en un archivo todos los string que hemos ido guardando con la informacion de las rutas que hemos ido obteniendo

--Funcion Ruta Distancia Ortodromica radianes: Calcula la distancia ortodromica entre el origen (x,y) y el destino (z,t) en radianes
-----------------------------------------------(Funciona)

distancia_ort (x,y) (z,t) = 6371*acos(s1)
  where s1= (sin(y)*sin(t))+(cos(y)*cos(t)*cos(c1))
        c1=z-x
--Funcion grados a radianes: Funcion auxiliar que usaremos para pasar de grados a radianes
------------------------------------(Funciona)

rad2grad x = 180*x/pi
--Funcion radianes a grados: Funcion auxiliar que usaremos para pasar de radianes a grados 
------------------------------------(Funciona)

grad2rad x = x*pi/180
 --Funcion Ruta Distancia Ortodromica Grados: Calcula la distancia ortodromica entre el origen (x,y) y el destino (z,t) en grados
-----------------------------------------------(Funciona)

distancia_ort_f (x,y) (z,t) = distancia_ort (x1,y1) (z1,t1)
  where x1= grad2rad x
        y1= grad2rad y
        z1= grad2rad z
        t1= grad2rad t

--Rumbo Ortodromica Ida Rad : Calcula la rumbo ortodromica en rad de la ida desde origen (x,y) hasta destino (z,t)
-------------------------------------- (Funciona)
rumbo_ort_1 (x,y) (z,t) = atan2 s1 s2
 where s1=cos(t)*sin(c1)
       s2=cos(y)*sin(t)-sin(y)*cos(t)*cos(c1)
       c1=z-x


--Rumbo Ortodromica Ida Grados Calcula la rumbo ortodromica en grados de la ida desde origen (x,y) hasta destino (z,t)
-------------------------------------- (Funciona)
rumbo_ort_f1 (x,y) (z,t) = rad2grad sf
  where x1= grad2rad x
        y1= grad2rad y
        z1= grad2rad z
        t1= grad2rad t
        sf= rumbo_ort_1 (x1,y1) (z1,t1)
--Rumbo Ortodromica Vuelta Rad: Calcula la rumbo ortodromica en rad de la ida desde origen (z,t) hasta destino (x,y)
-------------------------------------- (Funciona)
rumbo_ort_2 (x,y) (z,t) = atan2 s1 s2
  where s1=cos(y)*sin(c1)
        s2=cos(t)*sin(y)-sin(t)*cos(y)*cos(c1)
        c1=z-x


--Rumbo Ortodromica Vuelta Grados: Calcula la rumbo ortodromica en grados de la ida desde origen (z,t) hasta destino (x,y)
-------------------------------------- (Funciona)
rumbo_ort_f2 (x,y) (z,t) = rad2grad sf
  where x1= grad2rad x
        y1= grad2rad y
        z1= grad2rad z
        t1= grad2rad t
        sf= rumbo_ort_2 (x1,y1) (z1,t1)




--Proyeccion Mecator solo y: Utilizamos la proyecion de mercator pero solo se aplica a la coordenada y porque es la que necesitaremos 
---------------------------------(Funciona)
mercator (x,y)= (x1,y1)
  where x1=x
        y1=ln
        ln=log (tan(pi/4+y/2))


--Proyeccion Mecator solo y grados: Lo mismo que la anterior pero partiendo de  grados 
---------------------------------(Funciona)
mercator_f (x,y)= mercator (x1,y1)
  where x1= grad2rad x
        y1= grad2rad y

--Rumbo Loxodromico  : Calcula la rumbo loxodormico en grados de la ida desde origen (x,y) hasta destino (z,t)
-------------------------------(Funciona)
rumbo_lox_1 (x,y) (z,t) = if abs(ss)<180 then rad2grad (atan2 s1 s2) else rad2grad(atan2 s3 s2)
  where (x1,y1)=mercator_f (x,y)
        (z1,t1)=mercator_f (z,t)
        s1=z1-x1
        s2=t1-y1
        ss=z-x
        s3= if ss<=0 then grad2rad(360-abs(ss)) else -(grad2rad(360-abs(ss))) 
      
--Rumbo Loxodromico :  Calcula la rumbo loxodormico en grados de la vuelta desde origen (z,t) hasta destino (x,y) 
-------------------------------------(funciona)
rumbo_lox_2 (x,y) (z,t) = if abs(ss)<180 then rad2grad (atan2 s1 s2) else rad2grad(atan2 s3 s2)
  where (x1,y1)=mercator_f (x,y)
        (z1,t1)=mercator_f (z,t)
        s1=x1-t1
        s2=y1-t1
        ss=x-z
        s3= if ss<=0 then grad2rad(360-abs(ss)) else -(grad2rad(360-abs(ss))) 
      
--Ruta Loxodromica: Se calcula la distancia de la ruta loxodromica entre origen (x,y) y destino (z,t)
-------------------------------(Funciona)
ruta_lox_2(x,y) (z,t) = s1
  where x1= grad2rad x
        y1= grad2rad y
        z1= grad2rad z
        t1= grad2rad t
        s1=abs((t1-y1)/cos(s2)*6371)
        s2=grad2rad(rumbo_lox_1 (x,y) (z,t) )







--Calcular_vector: Se calcula el vector de las en el plano de las coordenada (x,y)
--------------------------(funciona)(No se Usa)
calcular_vector (x,y) = (xf,yf,zf)
  where (xf,yf,zf) =(6371*(cos(y)*cos(x)),6371*(cos(y)*sin(x)),6371*(sin(y)))



--Punto intermedio para calcular el punto intermedio entre 2 coordenadas (x,y) y (z,t) en radianes 
-------------------------(funciona)(No se Usa)
punto_intermedio (x,y) (z,t)= (lon,lat)
  where (x1,y1,z1)= calcular_vector (x,y)
        (x2,y2,z2)=calcular_vector (z,t)
        (xt,yt,zt)=(x1+x2,y1+y2,z1+z2)
        lon= atan2 yt xt
        lat= atan (zt/sq)
        sq=sqrt(xt*xt+yt*yt)


--Punto intermedio  para calcular el punto intermedio entre 2 coordenadas (x,y) y (z,t) en grados
---------------------------------(funciona)(No se Usa)
punto_intermedio_f (x,y) (z,t)= (rad2grad s1, rad2grad s2)
  where x1= grad2rad x
        y1= grad2rad y
        z1= grad2rad z
        t1= grad2rad t
        (s1,s2)= punto_intermedio (x1,y1) (z1,t1)


-- Metodo ausxiliar para pasar una distancia de km a  Radios ecuatoriales del mundo
---------------------------------(funciona)
d_angular d = d/6371 


--Metodo para calcular puntos intermedios se utilizara la como fuente la Aviation Formulary de Ed Williams, para calcular la longitud y latitud de un punto intermedio entre 2 puntos (x,y) y (t,z)   a traves de una fraccion de la distancia (d)  entre ellos. Siendo f la fraccion de la distancia siendo f=0 el punto inicial y f=1 el punto final. Los 2 puntos no pueden ser antipodas:
 {-- A=sin((1-f)*d)/sin(d)
        B=sin(f*d)/sin(d)
        x = A*cos(lat1)*cos(lon1) +  B*cos(lat2)*cos(lon2)
        y = A*cos(lat1)*sin(lon1) +  B*cos(lat2)*sin(lon2)
        z = A*sin(lat1)           +  B*sin(lat2)
        lat=atan2(z,sqrt(x^2+y^2))
        lon=atan2(y,x)

--}


--------------------------------------------------(funciona)
metodo2 d f = (a1,b1)
  where a1=sin((1-f)*d1)/sin(d1)
        b1=sin(f*d1)/sin(d1)
        d1=d_angular d
   
metodo2_f d f (x,y) (z,t) = (lon,lat)
  where x1 = a1*cos(y)*cos(x) +  b1*cos(t)*cos(z)
        y1 = a1*cos(y)*sin(x) +  b1*cos(t)*sin(z)
        z1 = a1*sin(y) +  b1*sin(t) 
        lat= atan2 z1 sq
        sq=sqrt(x1*x1+y1*y1)
        lon= atan2 y1 x1 
        (a1,b1)= metodo2 d f
--Mismo metodo pero partiendo de coordenadas dadas en grados
--------------------------------------------------(funciona)
metodo2_final d n (x,y) (z,t) = (a,b)
  where (a,b)= metodo2_f d n (x1,y1) (z1,t1)
        x1= grad2rad x
        y1= grad2rad y
        z1= grad2rad z
        t1= grad2rad t

--Metodo recursivo para calcular todos los puntos intermededios, empezando por el punto f=0 pero sin calcular el tultimo menos el ultimo de la ruta
-- d= distancia entre los 2 puntos entre km, a= el punto desde el que se parte, n= la fraccion por la que se van a ir haciendo los cortes. (x,y) = coordenadas iniciales, (z,t) = coordenadas finales. Es el metodo recursivo 
-------------------------------------------------------------(funciona)
puntos_intermedios_2 d 0 n (x,y) (z,t) = []
puntos_intermedios_2 d a n (x,y) (z,t) = ((x1,y1):puntos_intermedios_2 d a1 n (x,y) (z,t) )
  where (x1,y1)= metodo2_final d a1 (x,y) (z,t)
        a1= a-n
--Calculo todos los puntos intermedios con el ultimo de la ruta incluido
--Siendo p la cantidad de puntos intermedios (x,y) = coordenadas iniciales, (z,t) = coordenadas finales. 
-------------------------------------------------------------(funciona)
lista_puntos_final p (x,y) (z,t) = reverse((z1,t1):xs)
  where xs=puntos_intermedios_2 d a n (x,y) (z,t) --Se usa el metodo recursivo para la lista de puntos intermedios
        (z1,t1)=(grad2rad z, grad2rad t)
        d=distancia_ort_f (x,y) (z,t) -- Se calcular la distancia ortodromica entre 2 puntos  
        a=1 -- Se parte con a igual a 1 , al que se ira restando la fraccion n para calcular los diferentes puntos 
        n= 1/(p+1) -- Valor de la fraccion dependiendo de los los p puntos iniciales


--- Pasar una lista de Rad a Grados 
------------------------------------------------(funciona) 
lista_rad2grad []= []
lista_rad2grad ((x,y):xs)= (x1,y1):lista_rad2grad xs
  where (x1,y1)= (rad2grad x, rad2grad y)

---------------------------------Hasta aqui

---Distancias Poliloxidromica : Se calcula la distancia loxodromica de los puntos de una lista de manera recursiva
--------------------------------------------------------(funciona)
distancias_pol []=[]
distancias_pol [(x,y)]=[]
distancias_pol ((x,y):(z,t):xs) = (s1: distancias_pol ((z,t):xs))
  where s1= ruta_lox_2 (x,y) (z,t)

---rumbos Poliloxidromica: Se calcula el rumbo loxodromica de los puntos de una lista de manera recursiva
--------------------------------------------------------(funciona)
rumbos_pol []=[]
rumbos_pol [(x,y)]=[]
rumbos_pol ((x,y):(z,t):xs) = (s1: rumbos_pol ((z,t):xs))
  where s1= rumbo_lox_1 (x,y) (z,t)
---Diferencia Poliloxidromica// Ortodromica: Se calcula la difencia entre la distancia ortodromica y la poliloxidromica entre 2 puntos (x,y) y (z,t) siendo  p los  puntos intermedios de la Poliloxidromica
---------------------------------------------------------(funciona)

diferencia_pol_ort p (x,y) (z,t) = sum(distancias_pol (lista_rad2grad(lista_puntos_final p (x,y) (z,t)))) -s1
 where s1=distancia_ort_f (x,y) (z,t)





























