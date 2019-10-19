module center

contains

   subroutine rcent(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3)

        implicit none
        
        real,intent(in), dimension(20) :: a1,a2,a3
        real,intent(in), dimension(40) :: c1,c2,c3,b1,b2,b3
        real,intent(out), dimension(20) ::d1,d2,d3
        integer i

        do i = 1, 20
            d1(i) = (a1(i) + b1(i) +b1(i+1) + c1(i) +c1(i+1))/(5.0)
            d2(i) = (a2(i) + b2(i) +b2(i+1) + c2(i) +c2(i+1))/(5.0)
            d3(i) = (a3(i) + b3(i) +b3(i+1) + c3(i) +c3(i+1))/(5.0)
        end do                                                
                                                              
    end subroutine rcent                                  

end module center
