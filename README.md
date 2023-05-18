# ProgFunc
Practica 4 de programación funcional. Jugando con las palabras

Funcionamiento general del codigo:
Primero se generarán toda las combinaciones posibles con las letras disponibles. A continuación se eliminaran las que no cumplan unas características generales (como que no es posible que haya 4 consonantes seguidas) 
y luego se comprobará que cumplan otras reglas relacionadas a las sílabas (como que consonantes al final de sílaba solo puede ser l, n, s,r). Estas reglas de las silabas se harán en profundiad, es decir que dada la palabra se intentara separar en sílabas 
de todas la maneras posibles de manera que cumplan con las reglas llegando al final de una separación antes de probar con otra y si encuentra una separación que satisface las reglas no se busca más.
Reglas generales:
-No puede haber 4 consonantes seguidas
...........
Reglas de sílabas:
-consonantes al final de sílaba solo puede ser l, n, s,r
..........
