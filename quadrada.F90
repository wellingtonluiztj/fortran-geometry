!-----------------------------------------------------------
! Program: Sphere Geometry Construction in Square Lattice
! Author: Wellington Santos
! Date: December 6, 2023
! Description: This program creates a geometry of spheres
!              in a square lattice for simulation purposes.
!-----------------------------------------------------------
      PROGRAM GeometriaQuadrada
              IMPLICIT NONE
              ! Variables
              REAL :: r
              INTEGER :: i,j,u,v,n,m
              INTEGER, PARAMETER :: dim1 = 430
              INTEGER, PARAMETER :: dim2 = 430
              CHARACTER(len=12) :: quad
              REAL :: raio
              !Criando a matriz
              REAL :: matriz(dim1,dim2)
              !Origem
              REAL, DIMENSION(5) :: x0
              REAL, DIMENSION(5) :: y0
            

              !Matriz de zeros
              matriz = 0.0
              raio = 20
              !Array com x0 apenas
              DO u = 1, 5
              x0(u) = (u * 80) + 10
              y0(u) = (u * 80) + 10
              END DO

              
              !Nome do arquivo de saída
              quad = "quadrada.txt"
              PRINT *, x0
              !Abrir o arquivo de escrita
              OPEN(UNIT=20, FILE=quad,STATUS='REPLACE',ACTION='WRITE')
              
              DO n = 1, 5
              DO m = 1, 5
              DO i = 1, dim1, 1
              DO j = 1, dim2, 1
              r = SQRT((x0(n)-i)**2 + (y0(m)-j)**2)
              IF (r < raio) THEN
                      matriz(i,j) = 1.0
                      IF (matriz(i,j) == 1.0) THEN
                      WRITE(20,'(I0,1X,I0)') i+10,j
              ENd IF
              END IF
              END DO
              END DO
              END DO
              END DO
              CLOSE(20)

              END PROGRAM GeometriaQuadrada
