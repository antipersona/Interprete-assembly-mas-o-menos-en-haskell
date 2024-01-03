
@ .text
@ .global _start

_start:
    ; Supongamos que queremos calcular el factorial de 5.
    mov r0, #5      ; Coloca el número 5 en el registro r0

    bl factorial   ; Llama a la función factorial
    ; En este punto, el resultado (en r0) se encuentra en el registro r0

    ; Salida: aquí puedes agregar código para mostrar o utilizar el valor en r0
    ; Por ejemplo, podrías llamar a una función para imprimir el valor en r0

    @ ; Código para salir
    @ mov r7, #1      ; syscall para salir (exit)
    @ mov r0, #0      ; código de salida 0
    @ svc #0          ; llamada al sistema

factorial:
    ; Supongamos que el valor n está en r0
    ; Si n es 0 o 1, el factorial es 1.
    cmp r0, #1
    ble end_factorial

    ; Guarda el valor actual de r0 en la pila
    push {r0}

    ; Llama a factorial(n - 1)
    sub r0, r0, #1  ; n - 1
    bl factorial

    ; Recupera el valor de r0 (n - 1) de la pila
    pop {r1}

    ; Calcula n * factorial(n - 1)
    mul r0, r0, r1

end_factorial:
    bx lr           ; Retorno de función

