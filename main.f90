#define _pr 8

module config
    implicit none
    public

    !Segment boundaries
    real(_pr), parameter :: min = 0, max = 1
    !Integration prescision
    real(_pr), parameter :: prec = .000000001
    !Plot smoothness
    integer, parameter :: dots = 100
end module config

program output
    use config
    implicit none

    procedure(func), pointer :: fptr
    fptr => func

    print *, 'Integrating...'
    print *, 'Averages:     ', aver(fptr, min, max, prec)
    print *, 'Tprapezies:   ', trapez(fptr, min, max, prec)
    print *, 'Simpsons:     ', simp(fptr, min, max, prec)
    print *, 'Three Eights: ', threeEights(fptr, min, max, prec)

contains
    function func(arg) result(res)
        real(_pr), intent(in) :: arg
        real(_pr) :: res
        res = arg**1
    end function func

    function aver(fptr, min, max, step) result(res)
        procedure(func), pointer, intent(in) :: fptr
        real(_pr), intent(in) :: min, max, step

        real(_pr) :: x, sstep
        real(_pr) :: res, sres, er, next
        integer :: n, i

        !Step fix
        n = (max-min)/step
        sstep = (max-min)/n

        x = min
        res = 0
        do i = 0, n-1
            next = (sstep)*(fptr(x+sstep/2)) - er
            sres = res + next
            er = (sres - res) - next
            res = sres
            x=x+sstep
        end do
    end function aver

    function trapez(fptr, min, max, step) result(res)
        procedure(func), pointer, intent(in) :: fptr
        real(_pr), intent(in) :: min, max, step

        real(_pr) :: x, sstep
        real(_pr) :: res, sres, er, next
        integer :: n, i

        !Step fix
        n = (max-min)/step
        sstep = (max-min)/n

        x = min
        res = 0
        do i = 0, n-1
            next = (sstep/2)*(fptr(x)+fptr(x+sstep)) - er
            sres = res + next
            er = (sres - res) - next
            res = sres
            x=x+sstep
        end do
    end function trapez

    function simp(fptr, min, max, step) result(res)
        procedure(func), pointer, intent(in) :: fptr
        real(_pr), intent(in) :: min, max, step
        real(_pr) :: x, sstep
        real(_pr) :: res, sres, er, next
        integer :: n, i

        !Step fix
        n = (max-min)/step
        n = n + mod(n,2)
        sstep = (max-min)/n

        x = min
        res = 0
        er = 0
        do i = 0, n-1, 2
            next = (sstep/3)*(fptr(x)+4*fptr(x+sstep)+fptr(x+2*sstep)) - er
            sres = res + next
            er = (sres - res) - next
            res = sres
            x=x+2*sstep
        end do
    end function simp

    function threeEights(fptr, min, max, step) result(res)
        procedure(func), pointer, intent(in) :: fptr
        real(_pr), intent(in) :: min, max, step
        real(_pr) :: x, sstep
        real(_pr) :: res, sres, er, next
        integer :: n, i

        !Step fix
        n = (max-min)/step
        n = n + 2*mod(n,3)
        sstep = (max-min)/n

        x = min
        res = 0
        er = 0
        do i = 0, n-1, 3
            next = (3*sstep/8)*(fptr(x)+3*fptr(x+sstep)+3*fptr(x+2*sstep)+fptr(x+3*sstep)) - er
            sres = res + next
            er = (sres - res) - next
            res = sres
            x=x+3*sstep
        end do
    end function threeEights
end program output

