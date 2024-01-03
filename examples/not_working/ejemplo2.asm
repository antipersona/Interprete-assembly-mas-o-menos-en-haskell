.global _start

.section .data
mensaje:
    .asciz "El resultado es: %d\n"

.section .text
_start:
    // Suponiendo que r0 y r1 contienen los números que queremos sumar.
    // Suma r0 y r1
    add r2, r0, r1

    // Llamada al sistema para escribir el mensaje en la pantalla
    ldr r0, =mensaje   // r0 = dirección del mensaje
    mov r1, r2         // r1 = resultado de la suma
    bl printf          // Llama a la función printf

    // Salida del programa
    mov r7, #1         // Número de llamada al sistema para exit (1)
    mov r0, #0         // Código de retorno
    svc 0              // Llamada al sistema
