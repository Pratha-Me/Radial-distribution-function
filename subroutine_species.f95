module species 

contains    

   !rdf IS THE OUTPUT                                   
    subroutine radfunc(x1,y1,z1,natm1, x2,y2,z2,rdf)
        
        implicit none
        
        real, parameter :: c = 8.752448, dr = 0.1, pi = 2*asin(1.0)
        real,intent(in), dimension(natm1) :: x1,y1,z1,x2,y2,z2
        real, intent(out), dimension(1:int(c/dr)) :: rdf
        real r,al,gen_dens

        integer, intent(in) :: natm1
        integer, parameter :: n_cl = 20
        integer i,j,p,at_count

        at_count = 0; al = 0
        ! THE GENERAL DENSITY CALCULATION
        do i = 1, natm1
        do j = 1, n_cl

           al = sqrt((x1(i)-x2(j))**2+(y1(i)-y2(j))**2+(z1(i)-z2(j))**2)

            if (al .le. c) then
                at_count = at_count +1
            end if

        end do
        end do
        gen_dens = 3*at_count/(4*pi*c**3)
        !----------------------------------------------------

        ! THE RADIAL DENSITY DISTRIBUTION FUNCTION CALCULATION
        at_count = 0; al = 0; r = dr; p = 1
        do while(r .le. c)

        do i = 1, natm1
        do j = 1, n_cl
            al = sqrt((x1(i)-x2(j))**2+(y1(i)-y2(j))**2+(z1(i)-z2(j))**2)

            if (al .gt. r .and. al .le. r+dr) then
                at_count = at_count +1
            end if
        end do
        end do

        rdf(p) = at_count/(4*pi*r**2*dr*gen_dens)
        p = p +1; r = r + dr
        at_count = 0

        end do
        !----------------------------------------------------

    end subroutine radfunc

end module rdf
