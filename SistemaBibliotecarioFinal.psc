Algoritmo SistemaBibliotecarioFinal
	// 1. DIMENSIÓN DE MATRICES
	Definir autores, libros, socios, prestamos Como Cadena
	Dimensionar autores(101,3), libros(101,6), socios(101,5), prestamos(501,6)
	Definir cAutores, cLibros, cSocios, cPrestamos Como Entero
	Definir opcion_menu Como Cadena
	cAutores <- 0
	cLibros <- 0
	cSocios <- 0
	cPrestamos <- 0
	Repetir
		Limpiar Pantalla
		Escribir '========= MENÚ BIBLIOTECA ========='
		Escribir '1. Registrar Autor'
		Escribir '2. Registrar Libro'
		Escribir '3. Registrar Socio'
		Escribir '4. Gestionar Préstamo'
		Escribir '5. Gestionar Devolución'
		Escribir '6. Pagar Multa'
		Escribir '7. Ver Inventario'
		Escribir '8. Ver Socios'
		Escribir '9. Salir'
		Escribir 'Seleccione una opción:'
		Leer opcion_menu
		Según opcion_menu Hacer
		
	"1": 
		RegistrarAutor(autores, cAutores)
	"2": 
		RegistrarLibro(libros, autores, cLibros, cAutores)
	"3": 
		RegistrarSocio(socios, cSocios)
	"4": 
		GestionarPrestamo(libros, cLibros, socios, cSocios, prestamos, cPrestamos)
	"5": 
		GestionarDevolucion(libros, cLibros, socios, cSocios, prestamos)
	"6": 
		PagarMulta(socios, cSocios)
	"7": 
		VerInventario(libros, cLibros, autores, cAutores)
	"8": 
		VerSocios(socios, cSocios)
	"9":
		Escribir "Saliendo del sistema... ¡Que tenga un buen día!"
	De Otro Modo:
		Escribir ">>> ERROR: La opción ", opcion_menu, " no es válida"
FinSegún
		
		// Pausa inteligente: No pide tecla si el usuario ya decidió salir (opción 9)
		Si opcion_menu <> "9" Entonces
			Escribir ""
			Escribir "Presione cualquier tecla para continuar..."
			Esperar Tecla
		FinSi
		
	Hasta Que opcion_menu = "9"
FinAlgoritmo

// ==========================================
// MÓDULOS DE REGISTRO
// ==========================================
Función RegistrarAutor(autores Por Referencia,cA Por Referencia)
	cA <- cA+1
	Escribir 'Nombre del autor:'
	Leer autores[cA,2]
	autores[cA,1]<-'AUT'+ConvertirATexto(cA)
	Escribir 'Autor registrado con ID: ', autores[cA,1]
FinFunción

Función RegistrarLibro(libros Por Referencia,autores,cL Por Referencia,cA)
	Definir idA Como Cadena
	Definir i Como Entero
	Escribir 'Título del libro:'
	Leer libros[cL+1,2]
	Escribir 'ID del Autor:'
	Leer idA
	cL <- cL+1
	libros[cL,1]<-'LIB'+ConvertirATexto(cL)
	libros[cL,3]<-idA
	Escribir 'Cantidad inicial:'
	Leer libros[cL,4]
	libros[cL,5]<-'Disponible'
FinFunción

// Cesia
Función RegistrarSocio(socios Por Referencia,cS Por Referencia)
	cS <- cS+1
	Escribir 'Nombre del socio:'
	Leer socios[cS,2]
	socios[cS,1]<-'SOC'+ConvertirATexto(cS)
	socios[cS,3]<-'0'
	socios[cS,4]<-'0' // Libros poseidos
	Escribir '>> Registro exitoso. El ID asignado es: ', socios[cS,1] // Multa
FinFunción

// ==========================================
// MÓDULOS DE GESTIÓN (GABRIEL)
// ==========================================
Función GestionarPrestamo(libros Por Referencia,cL,socios Por Referencia,cS,prestamos Por Referencia,cP Por Referencia)
	Definir idS, idL Como Cadena
	Definir pS, pL, i, stk Como Entero
	Escribir '--- PROCESAR PRÉSTAMO ---'
	Escribir 'Ingrese ID del Socio:'
	Leer idS
	Escribir 'Ingrese ID del Libro:'
	Leer idL
	pS <- 0
	pL <- 0
	// 1. BUSCAR SOCIO Y LIBRO
	Para i<-1 Hasta cS Hacer
		Si socios[i,1]=idS Entonces
			pS <- i
		FinSi
	FinPara
	Para i<-1 Hasta cL Hacer
		Si libros[i,1]=idL Entonces
			pL <- i
		FinSi
	FinPara
	// 2. VALIDACIONES Y SALIDA DE DATOS
	Si pS>0 Y pL>0 Entonces
		stk <- ConvertirANumero(libros[pL,4])
		// Verificamos stock, límite de 3 libros y que no tenga multas
		Si stk>0 Y ConvertirANumero(socios[pS,3])<3 Y socios[pS,4]='0' Entonces
			// Registrar el préstamo
			cP <- cP+1
			prestamos[cP,1]<-idS
			prestamos[cP,2]<-idL
			prestamos[cP,3]<-'Activo'
			// Actualizar matrices
			libros[pL,4]<-ConvertirATexto(stk-1)
			
			// Si después de prestar el stock es 0, cambiar estado
			Si (stk - 1) = 0 Entonces
				libros[pL, 5] <- "Agotado"
			FinSi
			
			socios[pS,3]<-ConvertirATexto(ConvertirANumero(socios[pS,3])+1)
			// ============================================================
			// SALIDA DE DATOS SOLICITADA
			// ============================================================
			Limpiar Pantalla
			Escribir '**********************************************'
			Escribir '          COMPROBANTE DE PRÉSTAMO             '
			Escribir '**********************************************'
			Escribir 'SOCIO: ', socios[pS,2]
			Escribir 'LIBRO: ', libros[pL,2] // Muestra el nombre del socio
			Escribir '----------------------------------------------' // Muestra el título del libro
			Escribir '¡PRÉSTAMO REALIZADO CON ÉXITO!'
			Escribir ''
			Escribir 'AVISO IMPORTANTE:'
			Escribir 'Tiene un plazo de 7 DÍAS para devolver el libro.'
			Escribir 'De lo contrario, se aplicará una multa de $0.50'
			Escribir 'por cada día de retraso.'
			Escribir '**********************************************'
		SiNo
			Escribir '>>> ERROR: El socio tiene multas, alcanzó el límite de 3 libros o no hay stock.'
		FinSi
	SiNo
		Escribir '>>> ERROR: ID de Socio o Libro no encontrado.'
	FinSi
FinFunción

// GABRIEL OFICIAL
Subproceso GestionarDevolucion(libros Por Referencia, cL, socios Por Referencia, cS, prestamos Por Referencia)
	Definir idL, idS Como Texto
	Definir i, j, d Como Entero
	Definir m, multa_actual Como Real
	Definir encontrado Como Logico
	
	Escribir "--- PROCESAR DEVOLUCIÓN ---"
	Escribir "Ingrese ID del Libro a devolver:"
	Leer idL
	Escribir "Ingrese ID del Socio que lo devuelve:" 
	Leer idS
	
	encontrado <- Falso
	
	Para i <- 1 Hasta 500 Hacer
		// Validamos ID Libro, ID Socio y que el prestamo este Activo
		Si prestamos[i, 2] = idL Y prestamos[i, 1] = idS Y prestamos[i, 3] = "Activo" Entonces
			Si encontrado = Falso Entonces
				encontrado <- Verdadero
				prestamos[i, 3] <- "Finalizado"
				
				Escribir "Libro: ", idL, " del Socio: ", idS, " identificado."
				Escribir "Días totales que tuvo el libro:"
				Leer d
				
				// Lógica de Multas (Solo si pasa de 7 dias)
				Si d > 7 Entonces
					m <- (d - 7) * 0.50
					Para j <- 1 Hasta cS Hacer
						Si socios[j, 1] = idS Entonces
							multa_actual <- ConvertirANumero(socios[j, 4])
							socios[j, 4] <- ConvertirATexto(multa_actual + m)
							
							Limpiar Pantalla
							Escribir "**********************************************"
							Escribir "         ¡ALERTA DE RETRASO!                 "
							Escribir "**********************************************"
							Escribir "Socio: ", socios[j, 2]
							Escribir "Multa por este libro: $", m
							Escribir "Deuda total ahora: $", socios[j, 4]
							Escribir "----------------------------------------------"
							Escribir "AVISO: El socio debe pagar para prestar de nuevo."
							Escribir "**********************************************"
						FinSi
					FinPara
				Sino
					Escribir ">> Devolución a tiempo. Sin multas."
				FinSi
				
				// Actualizar Stock en la matriz de libros
				// Actualizar Stock en la matriz de libros
				Para j <- 1 Hasta cL Hacer
					Si libros[j, 1] = idL Entonces
						// Incrementamos el stock numérico
						cant_actual <- ConvertirANumero(libros[j, 4])
						libros[j, 4] <- ConvertirATexto(cant_actual + 1)
						
						// REGLA DE ORO: Si ya hay 1 o más, el estado debe ser Disponible
						Si (cant_actual + 1) > 0 Entonces
							libros[j, 5] <- "Disponible"
						FinSi
					FinSi
				FinPara
				
				// Buscamos al socio para bajar su contador de libros
				Para j <- 1 Hasta cS Hacer
					Si socios[j, 1] = idS Entonces
						libros_socio <- ConvertirANumero(socios[j, 3])
						socios[j, 3] <- ConvertirATexto(libros_socio - 1)
						Escribir "Libros restantes del socio: ", socios[j, 3]
					FinSi
				FinPara
			FinSi
		FinSi
	FinPara
	
	Si encontrado = Falso Entonces
		Escribir ">>> ERROR: No se encontro ese libro prestado a ese socio."
	FinSi
FinSubproceso

Función PagarMulta(socios Por Referencia,cS)
	Definir idS Como Cadena
	Definir i, posS Como Entero
	Definir monto, deuda Como Real
	Escribir 'ID Socio:'
	Leer idS
	posS <- 0
	Para i<-1 Hasta cS Hacer
		Si socios[i,1]=idS Entonces
			posS <- i
		FinSi
	FinPara
	Si posS>0 Entonces
		deuda <- ConvertirANumero(socios[posS,4])
		Escribir 'Deuda: $', deuda
		Si deuda>0 Entonces
			Escribir 'Monto a pagar:'
			Leer monto
			socios[posS,4]<-ConvertirATexto(deuda-monto)
			Escribir 'Pago realizado.'
		FinSi
	FinSi
FinFunción

Función VerInventario(libros,cL,autores,cA)
	Definir i Como Entero
	Escribir 'ID | TITULO | STOCK | ESTADO'
	Para i<-1 Hasta cL Hacer
		Escribir libros[i,1], ' | ', libros[i,2], ' | ', libros[i,4], ' | ', libros[i,5]
	FinPara
FinFunción

Función VerSocios(socios, cS)
Definir i Como Entero
Escribir "ID | NOMBRE | LIBROS | MULTA"
Escribir "--------------------------------"
Para i <- 1 Hasta cS Hacer
	Escribir socios[i,1], " | ", socios[i,2], " | Posee: ", socios[i,3], " | Deuda: $", socios[i,4]
FinPara
FinFunción
