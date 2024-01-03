// Uso: ./interprete factorial.asm <int>
# siendo <int> un entero positivo el cual va a ser calcilado su factorial

my_function:
  # recibimos el parametro en r0
  mov r2, r0    # lo copiamos a r2 ya que r0 va a ser el return
  
loop:
  subi r2, r2, #1  # restamos 1 a r2 (actua como contador)
  cmpi  r2, #0     # comparamos con 0
  beq finish       # si es menor, terminamos
  mul  r0, r0, r2  # multiplicamos lo que teniamos guardado por el contador

  b loop           # volvemos a empezar hasta que r2 sea 0
finish:
  b .              # finalizamos el programa