! This fortran script lists all non-zero Wigner's 3j symbols up to a maximum order
! 3j symbols are in the form (j1 j2 j3)
!                            (m1 m2 m3)
!
! Rules for 3j symbols to be different from 0 are:
! -ji in natural integers
! -sum rules : m1+m2+m3/=0 ,
! -triangle rule: abs(j1-j2)<=j3<=j1+j2

program main

    integer :: j1,j2,j3,m1,m2,m3,sortOrder,ordermaxinj

    100 print*,"What is the maximum order you allow?"
    read(*,*) ordermaxinj
    if ( ordermaxinj<0 ) then
        print*,"The maximum order must be >= 0"
        print*,
        goto 100
    end if

    write(*,*)
    write(*,*)
    write(*,'(a)')"Non-zero 3j symbols are given in the form"
    write(*,'(a)')"( j1 j2 j3, m1 m2 m3 )"
    write(*,'(a)')"======================"
    write(*,*)

    do sortOrder=0,ordermaxinj**2+ordermaxinj**2+(2*ordermaxinj)**2

        do j1=0,ordermaxinj
            do j2=0,ordermaxinj
                do j3= abs(j1-j2), j1+j2  ! triangle rule

                    if (j1**2+j2**2+j3**2/=sortOrder) cycle ! to print with increasing complexity in j1 j2 j3 and not by order of j3 then j2 then j1

                    do m1=-j1,j1
                        do m2=-j2,j2
                            do m3=-j3,j3

                                if ( .not. (m1+m2+m3==0)) cycle ! sum rule: m1 + m2 + m3 /= 0
                           
                                333 format ('(',i3,i3,i3,',',i3,i3,i3,')')
                                write(*,333) j1,j2,j3,m1,m2,m3

                            end do
                        end do
                    end do

                    write(*,*)

                end do
            end do
        end do

    end do

end program main
