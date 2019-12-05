MODULE Task
use omp_lib
implicit none
contains


    !Ищет координаты левого верхнего и правого нижнего углов подматрицы A, имеющей наибольшую сумму элементов.
    SUBROUTINE GetMaxCoordinates( A, x1, y1, x2, y2 )

    real(8), dimension(:,:), intent(in) :: A !Исходная матрица.
    integer, intent(out) :: x1, y1, x2, y2   !Нужные координаты.
    integer n                                !Число строк.
    integer m                                !Число столбцов( элементов в строке ).
    integer Left, Up                         !Левый верхний угол подматрицы.
    integer Right, Down                      !Правый нижний угол подматрицы.
    real(8), allocatable :: current_line(:)  !Строка, с которой мы работаем.
    real(8) current_sum                      !Хранит промежуточную сумму.
    real(8) max_sum                          !Максимальная сумма.

        n = size( A, dim = 2 )
        m = size( A, dim = 1 )

        allocate( current_line(m) )

        max_sum = A(1,1)

        x1 = 1
        y1 = 1

        x2 = 1
        y2 = 1

        !$omp parallel private( Up, Down, Left, Right, current_line, current_sum ) shared( A, x1, y1, x2, y2, max_sum )
        !$omp do schedule( dynamic )
        !Проходит по строкам.
        do Up = 1, n

            current_line = A(:,Up)

            do Down = Up, n
 
                !Берет строку и складывает ее с остальными строками матрицы. Результат передается GetMaxInArray...
                if( Down > Up ) then

                    current_line = current_line + A(:,Down)

                endif

                call GetMaxInArray( current_line, current_sum, Left, Right )

                !Если текущая сумма больше максимальной, то перезаписывает координаты и контрольную сумму.
                !$omp critical
                if( current_sum > max_sum ) then

                    max_sum = current_sum

                    x1 = Left
                    y1 = Up

                    x2 = Right
                    y2 = Down

                endif
                !$omp end critical

            end do

        end do
        !$omp end do
        !$omp end parallel

        deallocate( current_line )

    END SUBROUTINE GetMaxCoordinates


    !Берет на вход подотрезок, на выходе - левая и правая границы подматрицы, а также сумма элементов рабочей строки.
    SUBROUTINE GetMaxInArray( cur_l, Sum, L, R )

    real(8), intent(in), dimension(:) :: cur_l !Отрезок, с которым мы работаем.
    integer, intent(out) :: L                  !Левая граница подотрезка.
    integer, intent(out) :: R                  !Правая граница подотрезка.
    real(8), intent(out) :: Sum                !Контрольная сумма.
    real(8) cur_sum                            !Текущая сумма элементов отрезка.
    integer i                                  !Счетчик.
    integer minus_pos                          !Запоминает, где последний раз значение суммы было отрицательным.

        Sum = cur_l(1)
        L = 1
        R = 1
        cur_sum = 0
        minus_pos = 0

        !Складывает элементы отрезка.
        do i = 1, size( cur_l )

            cur_sum = cur_sum + cur_l(i)

            if( cur_sum > Sum ) then

                Sum = cur_sum
                L = minus_pos + 1
                R = i

            endif

            !Если текущая сумма сделалась отрицательной на i-том элементе, то приравнивает ее к нулю и minus_pos - к i.
            if( cur_sum < 0 ) then

                cur_sum = 0
                minus_pos = i

            endif

        enddo

    END SUBROUTINE GetMaxInArray


END MODULE Task



