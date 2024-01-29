PROGRAM GeometriaObliqua
    IMPLICIT NONE
    ! Variables
    REAL :: r
    INTEGER :: i, j, n, m, u, v
    INTEGER, PARAMETER :: dim1 = 430
    INTEGER, PARAMETER :: dim2 = 430
    CHARACTER(len=12) :: quad
    REAL :: raio
    ! Criando a matriz
    REAL :: matriz(dim1,dim2)
    ! Origem
    REAL, DIMENSION(5) :: x0
    REAL, DIMENSION(5) :: y0

    ! Matriz de zeros
    matriz = 0.0
    raio = 10

    ! Array com x0 e y0
    DO u = 1, 5
        x0(u) = (u-1) * 40   ! Ajuste para começar em 10 para linhas pares
        y0(u) = (u) * 40
    END DO

    ! Nome do arquivo de saída
    quad = "obliqua.txt"
    PRINT *, x0

    ! Abrir o arquivo de escrita
    OPEN(UNIT=20, FILE=quad, STATUS='REPLACE', ACTION='WRITE')

    DO n = 1, 5
        DO m = 1, 5
            DO i = 1, dim1, 1
                DO j = 1, dim2, 1
                    IF (MOD(n, 2) == 0) THEN
                        r = SQRT((x0(m) + 40 - i)**2 + (y0(n) - j)**2)
                    ELSE
                        r = SQRT((x0(m) + 20 - i)**2 + (y0(n) - j)**2)
                    END IF

                    IF (r < raio) THEN
                        matriz(i, j) = 1.0
                        IF (matriz(i, j) == 1.0) THEN
                            WRITE(20, '(I0,1X,I0)') i + 10, j
                        END IF
                    END IF
                END DO
            END DO
        END DO
    END DO

    CLOSE(20)
END PROGRAM GeometriaObliqua

